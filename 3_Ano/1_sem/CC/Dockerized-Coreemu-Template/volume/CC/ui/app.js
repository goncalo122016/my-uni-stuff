// DEFAULT SETTINGS
let API_HOST = "10.0.8.20";
const API_PORT = 5007;

function apiUrl(path) {
    return `http://${API_HOST}:${API_PORT}${path}`;
}

let pollInterval = 5000;
let pollTimer = null;

const $ = id => document.getElementById(id);

function setStatus(msg, ok = true) {
    console.log("[STATUS]", msg);
    const el = $("status");
    el.textContent = msg;
    el.style.color = ok ? "var(--accent)" : "red";
}

// API REQUEST
async function apiRequest(method, path, payload=null) {
    const url = apiUrl(path);
    //console.log(`[API] ${method} ${url}`, payload);

    try {
        const res = await fetch(url, {
            method,
            headers: { "Content-Type": "application/json" },
            body: payload ? JSON.stringify(payload) : undefined
        });

        const text = await res.text();

        let json = null;
        try {
            json = JSON.parse(text);
        } catch (e) {
            console.error("Falha a fazer JSON.parse:", e);
            return null;
        }

        //console.log("[JSON RESPONSE]", json);
        return json;

    } catch (err) {
        console.error("[API ERROR]", err);
        setStatus("Falha de comunicação com API", false);
        return null;
    }
}

// Rovers
async function updateRovers() {
    console.log("[UPDATE ROVERS]");

    const json = await apiRequest("GET", "/rovers");
    if (!json) {
        setStatus("Erro ao obter rovers", false);
        return;
    }

    if (json.event !== "rovers_snapshot") {
        console.warn("[ROVERS] Resposta inesperada:", json);
        setStatus("API retornou erro", false);
        return;
    }

    console.log("[ROVERS DATA]", json.data.rovers);
    renderRovers(json.data.rovers);
    setStatus(`Online — ${Object.keys(json.data.rovers).length} rovers`);
}

function renderRovers(rovers) {
    console.log("[RENDER ROVERS]", rovers);

    const container = $("roversList");
    container.innerHTML = "";

    if (!rovers || Object.keys(rovers).length === 0) {
        container.textContent = "Nenhum rover disponível.";
        return;
    }

    for (const [rid, r] of Object.entries(rovers)) {
        const el = document.createElement("div");
        el.className = "rover-card";

        el.innerHTML = `
            <strong>${rid}</strong>
            <span class="rover-status state-${r.state}">${r.state}</span>
            <div class="mt8">
                <div><b>Pos:</b> (${r.position.x.toFixed(1)}, ${r.position.y.toFixed(1)}, ${r.position.z.toFixed(1)})</div>
                <div><b>Missão:</b> ${r.mission_id || "Nenhuma"}</div>
                <div><b>Bateria:</b> ${r.battery}%</div>
            
                <div class="battery-bar">
                    <div class="battery-inner" style="width:${r.battery}%"></div>
                </div>
            </div>
        `;

        container.appendChild(el);
    }
}

// Missions
async function updateMissions() {
    console.log("[UPDATE MISSIONS]");

    const json = await apiRequest("GET", "/missions");
    if (!json) return;

    if (json.event !== "missions_snapshot") {
        console.warn("[MISSIONS] Resposta inesperada:", json);
        return;
    }

    console.log("[MISSIONS DATA]", json.data.missions);
    renderMissions(json.data.missions);
}

async function cancelMission(missionId) {
    const json = await apiRequest("POST", `/missions/${missionId}/cancel`);
    if (!json) {
        setStatus("Erro ao cancelar missão", false);
        return false;
    }
    if (json.event === "mission_aborted") {
        setStatus(`Missão ${missionId} cancelada`);
        return true;
    }
    setStatus(json.data?.message || "Falha ao cancelar missão", false);
    return false;
}

function renderMissions(missions) {
    console.log("[RENDER MISSIONS]", missions);

    const container = $("missionsList");
    container.innerHTML = "";

    if (!missions || Object.keys(missions).length === 0) {
        container.textContent = "Nenhuma missão encontrada.";
        return;
    }

    for (const [mid, m] of Object.entries(missions)) {
        const el = document.createElement("div");
        el.className = "mission-card";
        const progressPct = (m.progress * 100).toFixed(1);

        el.innerHTML = `
            <strong>${mid}</strong>
            <div><b>Rover:</b> ${m.rover_id || "—"}</div>
            <div><b>Status:</b> ${m.status}</div>
            <div><b>Tarefas:</b> ${m.tasks}</div>

            <div class="progress-bar">
                <div class="progress-inner" style="width:${progressPct}%"></div>
            </div>
            <div style="margin-top:4px; font-size:0.85rem; opacity:0.8;">
                Progresso: ${progressPct}%
            </div>

            <div class="mission-actions">
                <button class="btn-icon btn-cancel" data-mid="${mid}" title="Cancelar Missão">
                    Cancelar
                </button>
            </div>
        `;
        // desativar botão se não estiver ativa/pendente
        if (!["active", "assigned", "pending"].includes((m.status || "").toLowerCase())) {
            el.querySelector(".btn-cancel").disabled = true;
            el.querySelector(".btn-cancel").style.opacity = 0.6;
            el.querySelector(".btn-cancel").title = "Missão não pode ser cancelada";
        }

        container.appendChild(el);
    }

    // delegação de eventos para cancelar
    container.addEventListener("click", async (ev) => {
        const btn = ev.target.closest(".btn-cancel");
        if (!btn) return;
        const missionId = btn.getAttribute("data-mid");
        btn.disabled = true;
        btn.textContent = "A cancelar…";
        const ok = await cancelMission(missionId);
        if (ok) {
            // atualizar lista após cancelamento
            await updateMissions();
            await updateRovers();
        } else {
            btn.disabled = false;
            btn.textContent = "Cancelar";
        }
    }, { once: true }); // regista apenas uma vez por render
}

// TELEMETRIA

async function loadLatestTelemetry() {
    console.log("[TELEMETRY] Fetching latest...");

    const json = await apiRequest("GET", "/telemetry/latest?limit=40");

    if (!json || json.event !== "telemetry_history") {
        console.warn("Unexpected telemetry payload:", json);
        return;
    }

    renderTelemetryEvents(json.data);
}

function renderTelemetryEvents(allHistories) {
    const list = $("eventsList");
    list.innerHTML = "";

    if (!allHistories || Object.keys(allHistories).length === 0) {
        list.innerHTML = "<li>Nenhum evento recente.</li>";
        return;
    }

    const grid = document.createElement("div");
    grid.className = "events-columns";

    for (const [roverId, history] of Object.entries(allHistories)) {
        if (!Array.isArray(history) || history.length === 0) continue;

        const col = document.createElement("div");
        col.className = "rover-column";

        const header = document.createElement("h3");
        header.textContent = roverId;
        col.appendChild(header);

        const eventsWrap = document.createElement("div");
        eventsWrap.className = "rover-events";

        // history já vem limitada a 5 eventos
        const ordered = history.slice();

        for (const evt of ordered) {
            const card = document.createElement("div");
            card.className = "telemetry-card";

            const tsMs = Number(evt.timestamp) * 1000;
            const t = isNaN(tsMs) ? "—" : new Date(tsMs).toLocaleTimeString("pt-PT");

            const pos = evt.position || { x: 0, y: 0, z: 0 };

            const sensors = evt.sensor_status || {};

            console.log("[TELEMETRY EVENT SENSORS]", evt.sensor_status);


            const sensorIcon = (ok) =>
                `<span class="sensor-dot ${ok ? "ok" : "fail"}"></span>`;

            card.innerHTML = `
                <div class="telemetry-meta">${t} — ${evt.state}</div>
                
                <div class="telemetry-pos">
                    Pos: (${Number(pos.x).toFixed(2)}, ${Number(pos.y).toFixed(2)}, ${Number(pos.z).toFixed(2)})
                </div>

                <div>Bateria: ${evt.battery}%</div>
                <div>Temperatura: ${evt.temperature} °C</div>

                <div class="sensor-block">
                    <div class="sensor-row">
                        <b>Sensores:</b>
                    </div>

                    <div class="sensor-row">
                        Temp ${sensorIcon(sensors.temperature)}
                        | Bat ${sensorIcon(sensors.battery)}
                        | Pneus ${sensorIcon(sensors.tires)}
                        | Antena ${sensorIcon(sensors.antenna)}
                    </div>
                </div>
            `;

            eventsWrap.appendChild(card);
        }

        col.appendChild(eventsWrap);
        grid.appendChild(col);
    }

    list.appendChild(grid);
}

// Send Mission
async function sendMission(payload) {
    console.log("[SEND MISSION]", payload);

    const json = await apiRequest("POST", "/send-mission", payload);

    if (!json) {
        setStatus("Erro ao enviar missão", false);
        return null;
    }

    console.log("[MISSION RESPONSE]", json);

    if (json.status === "success") {
        setStatus("Missão enviada com sucesso");
        return json;
    }

    setStatus("Erro ao enviar missão", false);
    return null;
}

// atualiza mapa após obter rovers
const _origUpdateRovers = updateRovers;
updateRovers = async function() {
    const json = await apiRequest("GET", "/rovers");
    if (!json || json.event !== "rovers_snapshot") {
        setStatus("Erro ao obter rovers", false);
        return;
    }
    renderRovers(json.data.rovers);
    updateMapWithRovers(json.data.rovers);
    setStatus(`Online — ${Object.keys(json.data.rovers).length} rovers`);
};

// opcional: usar telemetria mais recente para ajustar posição
const _origLoadLatestTelemetry = loadLatestTelemetry;
loadLatestTelemetry = async function() {
    const json = await apiRequest("GET", "/telemetry/latest?limit=40");
    if (!json || json.event !== "telemetry_history") return;

    renderTelemetryEvents(json.data);

    // extrai última posição por rover (se existir) e atualiza mapa
    const latest = {};
    for (const [rid, history] of Object.entries(json.data || {})) {
        if (!history || history.length === 0) continue;
        const last = history[history.length - 1];
        latest[rid] = {
            position: last.position,
            battery: last.battery,
            state: last.state
        };
    }
    if (Object.keys(latest).length > 0) {
        const shaped = {};
        for (const [rid, v] of Object.entries(latest)) {
            shaped[rid] = {
                position: { x: v.position.x, y: v.position.y, z: v.position.z },
                battery: v.battery,
                state: v.state
            };
        }
    }
};

// POLLING
function startPolling() {
    console.log("[POLLING STARTED]");
    if (pollTimer) clearInterval(pollTimer);

    updateRovers();
    updateMissions();
    loadLatestTelemetry();

    pollTimer = setInterval(() => {
        updateRovers();
        updateMissions();
        loadLatestTelemetry();
    }, pollInterval);
}

// UI EVENTS
function wireUi() {
    $("btnConnect").addEventListener("click", () => {
        const host = $("apiHost").value.trim();

        console.log("[CONNECT] Host introduzido:", host);

        if (!host) {
            alert("Introduz um IP válido da MotherShip!");
            return;
        }

        API_HOST = host;
        setStatus("A conectar...");
        startPolling();
    });

    $("btnRefresh").addEventListener("click", () => {
        console.log("[REFRESH CLICK]");
        updateRovers();
        updateMissions();
    });

    $("missionForm").addEventListener("submit", async (ev) => {
        ev.preventDefault();

        const payload = {
            rover_id: $("formRoverId").value.trim(),
            area: {
                x1: parseFloat($("x1").value),
                y1: parseFloat($("y1").value),
                x2: parseFloat($("x2").value),
                y2: parseFloat($("y2").value),
            },
            tasks: $("tasks").value.trim(),
            duration: parseInt($("duration").value),
            progress_period: parseInt($("progressPeriod").value)
        };

        console.log("[MISSION FORM PAYLOAD]", payload);

        $("formResult").textContent = "A enviar…";

        const result = await sendMission(payload);

        console.log("[MISSION FORM RESULT]", result);
        $("formResult").textContent = "Enviado!";
    });
}

// inicia o canvas do mapa no load
window.addEventListener("load", () => {
    initMapCanvas();
});

window.addEventListener("load", () => {
    // Preenche o input com o IP por padrão
    $("apiHost").value = API_HOST;

    // Liga handlers da UI
    wireUi();

    setStatus("A conectar automaticamente…");
    startPolling();
});
