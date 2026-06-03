// MAPA DE ROVERS (Canvas) 
const MapState = {
    rovers: {},
    bounds: { minX: 0, minY: 0, maxX: 100, maxY: 100 },
    padding: { l: 42, r: 26, t: 20, b: 34 },
    dpr: Math.max(1, window.devicePixelRatio || 1),
    ctx: null,
    canvas: null,
    fixedPoints: {
        charging: { x: 50, y: 50 }
    }
};

function initMapCanvas() {
    const canvas = $("mapCanvas");
    if (!canvas) return;
    MapState.canvas = canvas;
    MapState.ctx = canvas.getContext("2d");
    resizeMapCanvas();
    window.addEventListener("resize", resizeMapCanvas);
    renderMap();
}

function resizeMapCanvas() {
    const c = MapState.canvas;
    if (!c) return;
    const rect = c.getBoundingClientRect();
    c.width = Math.max(1, Math.floor(rect.width * MapState.dpr));
    c.height = Math.max(1, Math.floor(rect.height * MapState.dpr));
    MapState.ctx.setTransform(1, 0, 0, 1, 0, 0);
    MapState.ctx.scale(MapState.dpr, MapState.dpr);
    renderMap();
}

function expandBounds(bounds, factor = 0.06) {
    const w = bounds.maxX - bounds.minX;
    const h = bounds.maxY - bounds.minY;
    const dx = Math.max(1e-6, w * factor);
    const dy = Math.max(1e-6, h * factor);
    return {
        minX: bounds.minX - dx,
        maxX: bounds.maxX + dx,
        minY: bounds.minY - dy,
        maxY: bounds.maxY + dy,
    };
}

function updateMapWithRovers(roversDict) {
    if (!roversDict) return;
    MapState.rovers = {};
    let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;

    for (const [rid, r] of Object.entries(roversDict)) {
        const x = Number(r.position?.x ?? 0);
        const y = Number(r.position?.y ?? 0);
        MapState.rovers[rid] = {
            x, y,
            battery: Number(r.battery ?? 0),
            state: r.state || "UNKNOWN"
        };
        if (isFinite(x) && isFinite(y)) {
            minX = Math.min(minX, x); maxX = Math.max(maxX, x);
            minY = Math.min(minY, y); maxY = Math.max(maxY, y);
        }
    }

    // incluir sempre o ponto fixo de carregamento nas bounds
    const cx = MapState.fixedPoints.charging.x;
    const cy = MapState.fixedPoints.charging.y;
    minX = Math.min(minX, cx);
    maxX = Math.max(maxX, cx);
    minY = Math.min(minY, cy);
    maxY = Math.max(maxY, cy);

    if (minX === Infinity) {
        minX = 0; minY = 0; maxX = 100; maxY = 100;
    }
    const minSize = 50;
    if (maxX - minX < minSize) { const cx2 = (minX + maxX) / 2; minX = cx2 - minSize/2; maxX = cx2 + minSize/2; }
    if (maxY - minY < minSize) { const cy2 = (minY + maxY) / 2; minY = cy2 - minSize/2; maxY = cy2 + minSize/2; }

    MapState.bounds = { minX, minY, maxX, maxY };
    MapState.bounds = expandBounds(MapState.bounds, 0.08);
    renderMap();
}

function worldToScreen(x, y) {
    const pad = MapState.padding;
    const c = MapState.canvas;
    if (!c) return { sx: 0, sy: 0 };
    const w = c.clientWidth - (pad.l + pad.r);
    const h = c.clientHeight - (pad.t + pad.b);
    const { minX, minY, maxX, maxY } = MapState.bounds;
    const rx = (x - minX) / Math.max(1e-6, (maxX - minX));
    const ry = (y - minY) / Math.max(1e-6, (maxY - minY));
    const sx = pad.l + rx * w;
    const sy = pad.t + (1 - ry) * h; // inverte Y para cima
    return { sx, sy };
}

function drawGrid(ctx) {
    const c = MapState.canvas;
    const pad = MapState.padding;
    const w = c.clientWidth, h = c.clientHeight;

    ctx.save();
    ctx.clearRect(0, 0, w, h);

    // fundo
    const grd = ctx.createLinearGradient(0, 0, 0, h);
    grd.addColorStop(0, "#0b1424");
    grd.addColorStop(1, "#0a1020");
    ctx.fillStyle = grd;
    ctx.fillRect(0, 0, w, h);

    // moldura do mapa
    ctx.strokeStyle = "rgba(255,255,255,0.08)";
    ctx.strokeRect(pad.l, pad.t, w - (pad.l + pad.r), h - (pad.t + pad.b));

    // grelha
    ctx.strokeStyle = "rgba(255,255,255,0.06)";
    ctx.lineWidth = 1;
    const steps = 8;
    const gx = (w - (pad.l + pad.r)) / steps;
    const gy = (h - (pad.t + pad.b)) / steps;

    ctx.beginPath();
    for (let i = 1; i < steps; i++) {
        ctx.moveTo(pad.l + i*gx, pad.t);
        ctx.lineTo(pad.l + i*gx, h - pad.b);
        ctx.moveTo(pad.l, pad.t + i*gy);
        ctx.lineTo(w - pad.r, pad.t + i*gy);
    }
    ctx.stroke();
    ctx.restore();
}

function drawAxesAndLabels(ctx) {
    const c = MapState.canvas;
    const pad = MapState.padding;
    const w = c.clientWidth, h = c.clientHeight;
    const { minX, minY, maxX, maxY } = MapState.bounds;

    ctx.save();
    ctx.fillStyle = "rgba(255,255,255,0.85)";
    ctx.strokeStyle = "rgba(255,255,255,0.25)";
    ctx.lineWidth = 1;
    ctx.font = "11px Inter, system-ui, sans-serif";

    const divs = 5;
    const stepX = (maxX - minX) / divs;
    const stepY = (maxY - minY) / divs;

    // Eixo X (baixo)
    for (let i = 0; i <= divs; i++) {
        const xVal = minX + i * stepX;
        const { sx } = worldToScreen(xVal, minY);
        ctx.beginPath();
        ctx.moveTo(sx, h - pad.b);
        ctx.lineTo(sx, h - pad.b + 6);
        ctx.stroke();
        const label = Number.isInteger(stepX) ? Math.round(xVal).toString() : xVal.toFixed(1);
        ctx.fillText(label, sx - ctx.measureText(label).width / 2, h - pad.b + 18);
    }

    // Eixo Y (esquerda)
    for (let i = 0; i <= divs; i++) {
        const yVal = minY + i * stepY;
        const { sy } = worldToScreen(minX, yVal);
        ctx.beginPath();
        ctx.moveTo(pad.l - 6, sy);
        ctx.lineTo(pad.l, sy);
        ctx.stroke();
        const label = Number.isInteger(stepY) ? Math.round(yVal).toString() : yVal.toFixed(1);
        ctx.fillText(label, 6, sy - 6);
    }

    // Legenda
    const legendX = w - pad.r - 128;
    const legendY = pad.t + 160;
    ctx.fillStyle = "rgba(0,0,0,0.35)";
    ctx.fillRect(legendX - 8, legendY - 8, 128, 48);
    ctx.strokeStyle = "rgba(255,255,255,0.12)";
    ctx.strokeRect(legendX - 8, legendY - 8, 128, 48);
    // Rover
    ctx.fillStyle = "#4ade80";
    ctx.beginPath(); ctx.arc(legendX, legendY, 5, 0, Math.PI*2); ctx.fill();
    ctx.fillStyle = "rgba(255,255,255,0.9)";
    ctx.fillText("Rover", legendX + 14, legendY - 5);
    // Charging
    ctx.fillStyle = "#f59e0b";
    ctx.fillRect(legendX, legendY + 14, 10, 10);
    ctx.fillStyle = "rgba(255,255,255,0.9)";
    ctx.fillText("Charging", legendX + 14, legendY + 22);

    ctx.restore();
}

function drawChargingPoint(ctx) {
    const p = MapState.fixedPoints.charging;
    const { sx, sy } = worldToScreen(p.x, p.y);
    ctx.save();
    ctx.fillStyle = "#f59e0b";
    ctx.strokeStyle = "rgba(0,0,0,0.5)";
    ctx.lineWidth = 1.5;
    ctx.beginPath();
    ctx.rect(sx - 7, sy - 7, 14, 14);
    ctx.fill();
    ctx.stroke();

    ctx.font = "12px Inter, system-ui, sans-serif";
    ctx.textBaseline = "bottom";
    ctx.fillStyle = "rgba(255,255,255,0.9)";
    const label = "Charging (50,50)";
    const w = ctx.measureText(label).width;
    const safeLeft = MapState.padding.l + 6;
    const safeRight = MapState.canvas.clientWidth - MapState.padding.r - 6;
    let lx = sx + 10;
    if (lx + w > safeRight) lx = sx - 10 - w;
    if (lx < safeLeft) lx = safeLeft;
    ctx.fillText(label, lx, sy - 10);
    ctx.restore();
}

function renderMap() {
    const ctx = MapState.ctx;
    const c = MapState.canvas;
    if (!ctx || !c) return;

    drawGrid(ctx);
    drawAxesAndLabels(ctx);
    drawChargingPoint(ctx);

    // desenha cada rover
    for (const [rid, r] of Object.entries(MapState.rovers)) {
        const { sx, sy } = worldToScreen(r.x, r.y);
        let color = "#4ade80";
        if (r.state === "IDLE") color = "#3b82f6";
        else if (r.state === "IN_MISSION") color = "#f59e0b";
        else if (r.state === "CHARGING") color = "#10b981";
        else if (r.state === "ERROR") color = "#ef4444";

        ctx.fillStyle = color;
        ctx.beginPath();
        ctx.arc(sx, sy, 6, 0, Math.PI * 2);
        ctx.fill();

        ctx.font = "12px Inter, system-ui, sans-serif";
        ctx.textBaseline = "top";
        ctx.fillStyle = "rgba(255,255,255,0.9)";
        ctx.fillText(rid, sx + 8, sy + 8);

        ctx.fillStyle = "rgba(255,255,255,0.6)";
        ctx.fillText(`${Math.round(r.battery)}%`, sx + 8, sy - 16);
    }
}