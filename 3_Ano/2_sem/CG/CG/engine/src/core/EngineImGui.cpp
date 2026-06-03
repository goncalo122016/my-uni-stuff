#include "Engine.hpp"
#include <imgui.h>
#include <imgui_impl_glfw.h>
#include <imgui_impl_opengl2.h>
#include <algorithm>

// ── Helpers ───────────────────────────────────────────────────────────────────

static void ShowHelpMarker(const char *desc)
{
    ImGui::TextDisabled("(?)");
    if (ImGui::IsItemHovered()) {
        ImGui::BeginTooltip();
        ImGui::PushTextWrapPos(450.0f);
        ImGui::TextUnformatted(desc);
        ImGui::PopTextWrapPos();
        ImGui::EndTooltip();
    }
}

static const char* getTransformTypeName(Transform::Type type)
{
    switch (type) {
        case Transform::Type::TRANSLATE: return "Translation";
        case Transform::Type::ROTATE:    return "Rotation";
        case Transform::Type::SCALE:     return "Scale";
        default:                         return "Unknown";
    }
}

// ── Group hierarchy ───────────────────────────────────────────────────────────

static void renderImGuiGroupHierarchy(Group& group, Group** followed, Vec3f* followed_prev_pos, Camera* camera)
{
    const auto& models     = group.getModels();
    const auto& children   = group.getChildren();
    const auto& transforms = group.getTransforms();

    bool is_followed = (*followed == &group);

    ImGuiTreeNodeFlags flags = 0;
    if (is_followed) flags |= ImGuiTreeNodeFlags_Selected;

    bool open = ImGui::TreeNodeEx(&group, flags, "Group (Models: %zu | Transforms: %zu | Children: %zu)",
        models.size(), transforms.size(), children.size());

    ImGui::SameLine();
    if (is_followed) {
        ImGui::PushStyleColor(ImGuiCol_Button, ImVec4(0.8f, 0.5f, 0.0f, 1.0f));
        if (ImGui::SmallButton("Unfollow"))
            *followed = nullptr;
        ImGui::PopStyleColor();
    } else {
        if (ImGui::SmallButton("Focus")) {
            *followed = &group;
            *followed_prev_pos = group.worldPos();
            camera->looking_at() = group.worldPos();
        }
    }

    if (!open) return;

    // Models
    if (!models.empty() && ImGui::TreeNode(&models, "Models (%zu)", models.size())) {
        for (size_t i = 0; i < models.size(); ++i)
            ImGui::BulletText("%s  [%zu verts]", models[i].GetName().c_str(), models[i].VertexCount());
        ImGui::TreePop();
    }

    // Transforms
    if (ImGui::TreeNode(&transforms, "Transformations (%zu)", transforms.size())) {
        if (ImGui::Button("Add Transformation"))
            ImGui::OpenPopup("AddTransformPopup");

        if (ImGui::BeginPopup("AddTransformPopup")) {
            if (ImGui::MenuItem("Translation"))  { group.addTranslate(0,0,0);         ImGui::CloseCurrentPopup(); }
            if (ImGui::MenuItem("Rotation"))     { group.addRotate(0,0,1,0);          ImGui::CloseCurrentPopup(); }
            if (ImGui::MenuItem("Scale"))        { group.addScale(1,1,1);             ImGui::CloseCurrentPopup(); }
            ImGui::EndPopup();
        }

        ImGui::Separator();
        auto& tv = const_cast<std::vector<Transform>&>(transforms);
        int del = -1, swap_a = -1, swap_b = -1;

        for (size_t i = 0; i < tv.size(); ++i) {
            Transform& t = tv[i];
            ImGui::PushID((int)i);
            ImGui::BeginGroup();

            ImGuiTreeNodeFlags tf = ImGuiTreeNodeFlags_DefaultOpen;
            bool topen = ImGui::TreeNodeEx(&t, tf, "[%zu] %s", i, getTransformTypeName(t.type));

            ImGui::SameLine(200);
            if (ImGui::SmallButton("X##del")) del = (int)i;
            ImGui::SameLine(230);
            if (i > 0 && ImGui::SmallButton("↑##up"))   { swap_a=(int)i; swap_b=(int)i-1; }
            ImGui::SameLine(258);
            if (i < tv.size()-1 && ImGui::SmallButton("↓##dn")) { swap_a=(int)i; swap_b=(int)i+1; }

            ImGui::EndGroup();

            if (topen) {
                ImGui::Indent();
                switch (t.type) {
                case Transform::Type::TRANSLATE:
                    if (t.time > 0) {
                        ImGui::Text("Catmull-Rom curve (%zu pts, time=%.2fs)", t.controlPoints.size(), t.time);
                        ImGui::DragFloat("Time##crt", &t.time, 0.1f, 0.01f, 1000.0f);
                        ImGui::Checkbox("Align to path##cr", &t.align);
                    } else {
                        ImGui::DragFloat3("Position##tr", &t.x, 0.1f);
                    }
                    break;
                case Transform::Type::ROTATE:
                    if (t.time > 0) {
                        ImGui::Text("Time-based rotation");
                        ImGui::DragFloat("Time to 360°##rot", &t.time, 0.1f, 0.01f, 1000.0f);
                        ImGui::DragFloat3("Axis##rotat", &t.x, 0.05f, -1.0f, 1.0f);
                    } else {
                        ImGui::DragFloat("Angle##rot", &t.angle, 1.0f, -360.0f, 360.0f);
                        ImGui::DragFloat3("Axis##rota", &t.x, 0.05f, -1.0f, 1.0f);
                    }
                    break;
                case Transform::Type::SCALE:
                    ImGui::DragFloat3("Scale##sc", &t.x, 0.05f, 0.001f, 100.0f);
                    break;
                }
                ImGui::Unindent();
                ImGui::TreePop();
            }
            ImGui::PopID();
        }

        if (del >= 0 && del < (int)tv.size()) tv.erase(tv.begin() + del);
        if (swap_a >= 0 && swap_b >= 0) std::swap(tv[swap_a], tv[swap_b]);

        ImGui::TreePop();
    }

    // Children
    if (!children.empty() && ImGui::TreeNode(&children, "Children (%zu)", children.size())) {
        auto& cv = const_cast<std::vector<Group>&>(children);
        for (auto& child : cv)
            renderImGuiGroupHierarchy(child, followed, followed_prev_pos, camera);
        ImGui::TreePop();
    }

    ImGui::TreePop();
}

// ── Lights section ────────────────────────────────────────────────────────────

static void renderImGuiLights(std::vector<Light>& lights)
{
    if (lights.size() > 8) {
        ImGui::PushStyleColor(ImGuiCol_Text, IM_COL32(255, 220, 0, 255));
        ImGui::TextWrapped("Aviso: apenas 8 luzes suportadas. As restantes sao ignoradas.");
        ImGui::PopStyleColor();
    }

    for (int i = 0; i < (int)lights.size(); ++i) {
        ImGui::PushID(i);
        ImGui::BeginGroup();

        std::visit([&](auto& light) {
            using T = std::decay_t<decltype(light)>;
            if constexpr (std::is_same_v<T, PointLight>) {
                ImGui::TextColored(ImVec4(1,0.9f,0.4f,1), "Light #%d  [Ponto]", i);
                ImGui::DragFloat3("Posicao##pl", &light.pos.x, 0.5f);
            } else if constexpr (std::is_same_v<T, DirectionalLight>) {
                ImGui::TextColored(ImVec4(0.5f,0.8f,1,1), "Light #%d  [Direcional]", i);
                ImGui::DragFloat3("Direcao##dl", &light.dir.x, 0.05f, -1.0f, 1.0f);
            } else if constexpr (std::is_same_v<T, Spotlight>) {
                ImGui::TextColored(ImVec4(1,0.5f,0.2f,1), "Light #%d  [Spotlight]", i);
                ImGui::DragFloat3("Posicao##sl",  &light.pos.x, 0.5f);
                ImGui::DragFloat3("Direcao##sld", &light.dir.x, 0.05f, -1.0f, 1.0f);
                ImGui::SliderFloat("Cutoff##slc", &light.cutoff, 0.0f, 90.0f);
            }
        }, lights[i]);

        ImGui::SameLine();
        if (ImGui::SmallButton("X##remlight")) {
            lights.erase(lights.begin() + i);
            ImGui::EndGroup();
            ImGui::PopID();
            return;
        }
        ImGui::EndGroup();
        ImGui::Separator();
        ImGui::PopID();
    }

    if (ImGui::Button("+ Adicionar Luz"))
        ImGui::OpenPopup("AddLightPopup");

    if (ImGui::BeginPopup("AddLightPopup")) {
        if (ImGui::MenuItem("Ponto")) {
            lights.push_back(PointLight{{0,0,0}});
            Lighting::SetupWorldLights(lights);
            ImGui::CloseCurrentPopup();
        }
        if (ImGui::MenuItem("Direcional")) {
            lights.push_back(DirectionalLight{{0,-1,0}});
            Lighting::SetupWorldLights(lights);
            ImGui::CloseCurrentPopup();
        }
        if (ImGui::MenuItem("Spotlight")) {
            lights.push_back(Spotlight{{0,10,0},{0,-1,0},45.0f});
            Lighting::SetupWorldLights(lights);
            ImGui::CloseCurrentPopup();
        }
        ImGui::EndPopup();
    }
}

// ── ImGui init / render / post ────────────────────────────────────────────────

void Engine::initImGui()
{
    IMGUI_CHECKVERSION();
    ImGui::CreateContext();
    io = &ImGui::GetIO();
    (void)io;
    io->ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;
    io->ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;

    ImGui::GetStyle().WindowRounding = 5.0f;
    ImGui::StyleColorsDark();

    ImGui_ImplGlfw_InitForOpenGL(m_window, true);
    ImGui_ImplOpenGL2_Init();
}

void Engine::renderImGui()
{
    ImGui_ImplOpenGL2_NewFrame();
    ImGui_ImplGlfw_NewFrame();
    ImGui::NewFrame();

    // Space = pause/resume animation
    if (ImGui::IsKeyPressed(ImGuiKey_Space)) {
        m_animation_paused = !m_animation_paused;
        if (m_animation_paused)
            m_paused_time = glfwGetTime();
    }

    ImGui::SetNextWindowPos(ImVec2(0, 0), ImGuiCond_FirstUseEver);
    ImGui::SetNextWindowSize(ImVec2(560, 700), ImGuiCond_FirstUseEver);

    ImGui::Begin(("Scene: " + m_scene.GetName()).c_str());

    // ── Window info ───────────────────────────────────────────────────────────
    {
        auto& win = m_scene.GetWindow();
        ImGui::Text("Janela: %d x %d", win.GetWidth(), win.GetHeight());
    }

    ImGui::Separator();

    // ── Animation ─────────────────────────────────────────────────────────────
    if (m_animation_paused) {
        ImGui::PushStyleColor(ImGuiCol_Button, ImVec4(0.8f, 0.2f, 0.2f, 1.0f));
        if (ImGui::Button("|| Pausa  [SPACE]", ImVec2(-1, 28)))
            m_animation_paused = false;
        ImGui::PopStyleColor();
    } else {
        ImGui::PushStyleColor(ImGuiCol_Button, ImVec4(0.15f, 0.6f, 0.15f, 1.0f));
        if (ImGui::Button(">  Play   [SPACE]", ImVec2(-1, 28))) {
            m_animation_paused = true;
            m_paused_time = glfwGetTime();
        }
        ImGui::PopStyleColor();
    }

    ImGui::Separator();

    // ── Camera ────────────────────────────────────────────────────────────────
    if (ImGui::TreeNodeEx("Camera", ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_DefaultOpen)) {
        auto& cam = m_scene.GetCamera();

        if (m_free_mode) {
            ImGui::PushStyleColor(ImGuiCol_Button, ImVec4(0.2f, 0.7f, 0.2f, 1.0f));
            if (ImGui::Button("Free Mode ON", ImVec2(-1, 28))) m_free_mode = false;
            ImGui::PopStyleColor();
            ImGui::TextDisabled("W/S/Setas: frente/tras  A/D: lateral  Q/E: cima/baixo");
            ImGui::TextDisabled("Btn direito: olhar  Btn esq: orbitar");
        } else {
            if (ImGui::Button("Free Mode OFF", ImVec2(-1, 28))) m_free_mode = true;
        }
        ImGui::SliderFloat("Velocidade##freecam", &m_free_mode_speed, 0.01f, 5.0f);
        if (ImGui::Button("Reset Camera", ImVec2(-1, 0))) m_scene.ResetCamera();

        ImGui::Separator();
        ImGui::DragFloat3("Posicao##cam",   &cam.position().x,   0.1f);
        ImGui::DragFloat3("LookAt##cam",    &cam.looking_at().x, 0.1f);
        ImGui::DragFloat3("Up##cam",        &cam.up().x,         0.01f);
        ImGui::SliderFloat("FOV##cam",      &cam.fov(),  1.0f, 179.0f);
        ImGui::SliderFloat("Near##cam",     &cam.near(), 0.001f, cam.far() - 0.01f);
        ImGui::SliderFloat("Far##cam",      &cam.far(),  cam.near() + 1.0f, 5000.0f);
        ImGui::TreePop();
    }

    ImGui::Separator();

    // ── Lights ────────────────────────────────────────────────────────────────
    if (ImGui::TreeNodeEx("Luzes", ImGuiTreeNodeFlags_Framed)) {
        auto& lights = m_scene.GetLights();
        renderImGuiLights(lights);
        ImGui::TreePop();
    }

    ImGui::Separator();

    // ── Groups ────────────────────────────────────────────────────────────────
    if (ImGui::TreeNodeEx("Grupos", ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_DefaultOpen)) {
        renderImGuiGroupHierarchy(m_scene.GetRootGroup(), &m_followed_group, &m_followed_prev_pos, &m_scene.GetCamera());
        ImGui::TreePop();
    }

    ImGui::Separator();

    // ── Settings ──────────────────────────────────────────────────────────────
    if (ImGui::TreeNodeEx("Definicoes", ImGuiTreeNodeFlags_Framed)) {

        if (ImGui::TreeNodeEx("OpenGL", ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_DefaultOpen)) {
            ImGui::Columns(2, nullptr, false);

            ImGui::Checkbox("Wireframe",    &m_settings.wireframe);
            ImGui::SameLine(); ShowHelpMarker("Renderiza apenas as arestas dos triangulos, sem preenchimento de faces");
            ImGui::NextColumn();
            if (ImGui::Checkbox("Cull Faces",   &m_settings.cull_faces))
                SetCullFaces(m_settings.cull_faces);
            ImGui::SameLine(); ShowHelpMarker("Descarta faces traseiras dos objectos para poupar GPU (backface culling)");
            ImGui::NextColumn();

            if (ImGui::Checkbox("VSync", &m_settings.vsync))
                SetVsync(m_settings.vsync);
            ImGui::SameLine(); ShowHelpMarker("Sincroniza a taxa de frames com o monitor para evitar tearing");
            ImGui::NextColumn();

            if (ImGui::Checkbox("Fullscreen", &m_settings.fullscreen))
                SetFullscreen(m_settings.fullscreen);
            ImGui::SameLine(); ShowHelpMarker("Expande a janela para ecra inteiro no monitor principal");
            ImGui::NextColumn();

            if (ImGui::Checkbox("Iluminacao", &m_settings.lighting))
                m_settings.lighting ? glEnable(GL_LIGHTING) : glDisable(GL_LIGHTING);
            ImGui::SameLine(); ShowHelpMarker("Activa o modelo de iluminacao Phong (ambient + diffuse + specular)");
            ImGui::NextColumn();

            ImGui::Checkbox("Frustum Culling", &m_settings.frustum_culling);
            ImGui::SameLine(); ShowHelpMarker("Ignora grupos fora do campo de visao da camara para poupar GPU");
            ImGui::NextColumn();

            ImGui::Columns(1);
            ImGui::TreePop();
        }

        if (ImGui::TreeNodeEx("Renderizacao Extra", ImGuiTreeNodeFlags_Framed | ImGuiTreeNodeFlags_DefaultOpen)) {
            ImGui::ColorEdit4("Cor de Fundo",   m_scene.GetBackgroundColorMutable());
            ImGui::SameLine(); ShowHelpMarker("Cor RGBA do fundo da cena (alpha nao tem efeito em modo opaco)");
            ImGui::Separator();
            ImGui::Columns(2, nullptr, false);
            ImGui::Checkbox("Eixos",            &m_settings.render_axis);
            ImGui::SameLine(); ShowHelpMarker("Mostra os eixos X (vermelho), Y (verde) e Z (azul) na origem");
            ImGui::NextColumn();
            ImGui::Checkbox("Orbitas",          &m_settings.render_orbits);
            ImGui::SameLine(); ShowHelpMarker("Desenha as curvas de Catmull-Rom das trajetorias animadas");
            ImGui::NextColumn();
            ImGui::Checkbox("AABBs",            &m_settings.render_aabb);
            ImGui::SameLine(); ShowHelpMarker("Mostra as caixas delimitadoras (AABB) de cada grupo — requer Frustum Culling activo");
            ImGui::NextColumn();
            ImGui::Checkbox("Normais",          &m_settings.render_normals);
            ImGui::SameLine(); ShowHelpMarker("Desenha os vectores normais de cada vertice (pode ser lento em cenas densas)");
            ImGui::NextColumn();
            ImGui::Columns(1);
            ImGui::TreePop();
        }

        if (ImGui::TreeNodeEx("Informacao do Sistema", ImGuiTreeNodeFlags_Framed)) {
            ImGui::BulletText("GLEW:   %s", m_system_environment.glew_version.c_str());
            ImGui::BulletText("GLFW:   %s", m_system_environment.glfw_version.c_str());
            ImGui::BulletText("ImGui:  %s", m_system_environment.imgui_version.c_str());
            ImGui::BulletText("OpenGL: %s", m_system_environment.opengl_version.c_str());
            ImGui::BulletText("GPU:    %s", m_system_environment.gpu_renderer.c_str());
            ImGui::TreePop();
        }

        ImGui::TreePop();
    }

    ImGui::Separator();
    ImGui::Text("%.3f ms/frame  (%.1f FPS)", 1000.0f / io->Framerate, io->Framerate);

    ImGui::End();
    ImGui::Render();
}

void Engine::postRenderImGui()
{
    ImGui_ImplOpenGL2_RenderDrawData(ImGui::GetDrawData());
}
