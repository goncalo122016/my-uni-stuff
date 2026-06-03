#include "Engine.hpp"
#include <cstdio>
#include <iostream>
#include <thread>
#include <chrono>
#include "../model/Model.hpp"

#include "imgui_impl_glfw.h"
#include "imgui_impl_opengl2.h"

Engine* Engine::g_engine_instance = nullptr;

void glfw_error_callback(int error, const char *desc)
{
    fprintf(stderr, "GLFW Error %d: %s\n", error, desc);
}

void glfwSetFramebufferSizeCallback(GLFWwindow *, int w, int h)
{
    glViewport(0, 0, w, h);
}

double last_scroll = 0;

void glfwScrollCallback(GLFWwindow *, double, double yo) { last_scroll = yo; }

bool Engine::Init()
{
    g_engine_instance = this;
    
    glfwSetErrorCallback(glfw_error_callback);
    if (!glfwInit()) return false;

    const int w = m_scene.GetWindow().GetWidth();
    const int h = m_scene.GetWindow().GetHeight();

    m_window = glfwCreateWindow(w, h, "CG Engine", nullptr, nullptr);
    if (!m_window) return false;

    glfwMakeContextCurrent(m_window);
    glfwSwapInterval(m_settings.vsync ? 1 : 0);
    glfwSetFramebufferSizeCallback(m_window, glfwSetFramebufferSizeCallback);
    glfwSetScrollCallback(m_window, glfwScrollCallback);

    if (const GLenum err = glewInit(); err != GLEW_OK) {
        std::cerr << "Glew Error: " << glewGetErrorString(err) << "\n";
        return false;
    }

    glEnable(GL_DEPTH_TEST);
    SetCullFaces(m_settings.cull_faces);

    // Enable client states once — kept permanently enabled.
    // Model::Render() only swaps buffer bindings, never calls Enable/Disable.
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_NORMAL_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glEnable(GL_TEXTURE_2D);

    // ========================================================================
    // INICIALIZACAO DO SISTEMA DE ILUMINACAO
    // ========================================================================
    
    // Habilitar iluminacao global (Fixed-Function Pipeline)
    glEnable(GL_LIGHTING);
    glEnable(GL_NORMALIZE);  // Normalizar vetores normais automaticamente
    
    // Inicializar sistema de luzes (desabilita todos os slots, seta parametros globais)
    setupWorldLights();

    // GL_COLOR_MATERIAL deliberately disabled: it would silently override all
    // glMaterialfv(AMBIENT/DIFFUSE) calls with the current glColor, making
    // per-model material properties ineffective.

    // Eagerly upload all model geometry and textures to the GPU.
    // This ensures there are no first-frame hitches and allows us to free the
    // CPU-side geometry cache (s_cpu_geo_cache) immediately afterwards.
    m_scene.GetRootGroup().initAllGPU();
    ClearModelCPUCache(); // s_cpu_geo_cache freed — GPU holds all geometry now

    initImGui();
    setupEnvironment();
    return true;
}

static void renderAxis()
{
    glDisable(GL_LIGHTING);
    glBegin(GL_LINES);
    glColor3f(1,0,0); glVertex3f(-1000,0,0); glVertex3f(1000,0,0);
    glColor3f(0,1,0); glVertex3f(0,-1000,0); glVertex3f(0,1000,0);
    glColor3f(0,0,1); glVertex3f(0,0,-1000); glVertex3f(0,0,1000);
    glColor3f(1,1,1);
    glEnd();
    glEnable(GL_LIGHTING);
}

static void renderCamera(const Camera &cam, const Window &win)
{
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    float aspect = (float)win.GetWidth() / (float)win.GetHeight();
    gluPerspective(cam.fov(), aspect, cam.near(), cam.far());
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    gluLookAt(cam.position().x, cam.position().y, cam.position().z,
              cam.looking_at().x, cam.looking_at().y, cam.looking_at().z,
              cam.up().x, cam.up().y, cam.up().z);
}

// ============================================================================
// IMPLEMENTACOES DAS FUNCOES DE ILUMINACAO
// ============================================================================

void Engine::setupWorldLights()
{
    /// @brief Inicializa o sistema de luzes OpenGL
    /// Chama o helper do namespace Lighting para configurar parametros globais
    Lighting::SetupWorldLights(m_scene.GetLights());
}

void Engine::renderLights()
{
    /// @brief Renderiza todas as luzes da cena num frame
    /// Chama o helper do namespace Lighting para aplicar cada luz
    /// IMPORTANTE: Deve ser chamado APOS configurar a camera (glLookAt)
    /// porque as posicoes das luzes sao transformadas para eye space
    Lighting::RenderLights(m_scene.GetLights());
}

void Engine::applyMaterial(const ModelMaterial &material)
{
    /// @brief Aplica o material de um modelo
    /// Chama o helper do namespace Lighting para configurar glMaterial*
    Lighting::ApplyMaterial(material);
}

void Engine::Render()
{
    renderImGui();

    // Update world positions and follow selected group.
    // When animation is paused the matrices are frozen — compute once, then skip.
    {
        const bool need_update = !m_animation_paused || !m_positions_valid;
        if (need_update) {
            float identity[16] = {1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1};
            m_scene.GetRootGroup().updateWorldPositions(identity);
        }
        m_positions_valid = m_animation_paused; // valid only while paused

        if (m_followed_group) {
            Vec3f new_pos = m_followed_group->worldPos();
            Vec3f delta   = new_pos - m_followed_prev_pos;
            m_scene.GetCamera().position()   = m_scene.GetCamera().position()   + delta;
            m_scene.GetCamera().looking_at() = m_scene.GetCamera().looking_at() + delta;
            m_followed_prev_pos = new_pos;
        }
    }

    glClearColor(m_scene.GetBackgroundColor()[0],
                 m_scene.GetBackgroundColor()[1],
                 m_scene.GetBackgroundColor()[2],
                 m_scene.GetBackgroundColor()[3]);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    renderCamera(m_scene.GetCamera(), m_scene.GetWindow());

    // ========================================================================
    // APLICACAO DAS LUZES NO FRAME
    // ========================================================================
    // Luzes devem ser aplicadas APOS configurar a camera (glLookAt)
    // porque as posicoes das luzes sao transformadas para eye space (camera space)
    // no pipeline OpenGL
    renderLights();

    if (m_settings.render_axis)
        renderAxis();

    glPolygonMode(GL_FRONT_AND_BACK, m_settings.wireframe ? GL_LINE : GL_FILL);

    // Re-enable client states every frame — ImGui's RenderDrawData disables them
    // at the end of each frame and glPushAttrib(GL_ENABLE_BIT) does not cover
    // client-state enables, so they are NOT automatically restored.
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_NORMAL_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);

    // Cache frustum for this frame (used by Group::render for frustum culling)
    if (m_settings.frustum_culling)
        m_cached_frustum = GetCurrentFrustum();

    m_scene.GetRootGroup().render();

    // ImGui OpenGL2 uses client-side glVertexPointer / glDrawElements with CPU
    // pointers. With a VBO still bound those pointers are treated as VBO offsets
    // and ImGui renders garbage (or nothing). One unbind per frame is far cheaper
    // than the per-model unbind we removed from Model::Render().
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    Model::InvalidateVBOCache(); // keep cache in sync with actual bound state

    postRenderImGui();
}

void Engine::SetWireframe(bool en) { m_settings.wireframe = en; }

void Engine::SetCullFaces(bool en)
{
    m_settings.cull_faces = en;
    en ? glEnable(GL_CULL_FACE) : glDisable(GL_CULL_FACE);
}

void Engine::SetVsync(bool en)
{
    m_settings.vsync = en;
    glfwSwapInterval(en ? 1 : 0);
}

void Engine::SetFullscreen(bool en)
{
    m_settings.fullscreen = en;
    if (en) {
        glfwGetWindowPos (m_window, &m_windowed_xpos, &m_windowed_ypos);
        glfwGetWindowSize(m_window, &m_windowed_width, &m_windowed_height);
        GLFWmonitor *mon = glfwGetPrimaryMonitor();
        const GLFWvidmode *mode = glfwGetVideoMode(mon);
        glfwSetWindowMonitor(m_window, mon, 0, 0, mode->width, mode->height, mode->refreshRate);
    } else {
        glfwSetWindowMonitor(m_window, nullptr,
                             m_windowed_xpos, m_windowed_ypos,
                             m_windowed_width, m_windowed_height, 0);
    }
}

void Engine::ProcessInput()
{
    static double lx = 0, ly = 0;
    double x, y;
    glfwGetCursorPos(m_window, &x, &y);

    if (m_free_mode) {
        if (!io->WantCaptureKeyboard) {
            float fwd = 0, strafe = 0, vert = 0;
            if (glfwGetKey(m_window, GLFW_KEY_W)         == GLFW_PRESS || glfwGetKey(m_window, GLFW_KEY_UP)        == GLFW_PRESS) fwd    += 1;
            if (glfwGetKey(m_window, GLFW_KEY_S)         == GLFW_PRESS || glfwGetKey(m_window, GLFW_KEY_DOWN)      == GLFW_PRESS) fwd    -= 1;
            if (glfwGetKey(m_window, GLFW_KEY_D)         == GLFW_PRESS || glfwGetKey(m_window, GLFW_KEY_RIGHT)     == GLFW_PRESS) strafe += 1;
            if (glfwGetKey(m_window, GLFW_KEY_A)         == GLFW_PRESS || glfwGetKey(m_window, GLFW_KEY_LEFT)      == GLFW_PRESS) strafe -= 1;
            if (glfwGetKey(m_window, GLFW_KEY_E)         == GLFW_PRESS || glfwGetKey(m_window, GLFW_KEY_PAGE_UP)   == GLFW_PRESS) vert   += 1;
            if (glfwGetKey(m_window, GLFW_KEY_Q)         == GLFW_PRESS || glfwGetKey(m_window, GLFW_KEY_PAGE_DOWN) == GLFW_PRESS) vert   -= 1;
            if (fwd != 0 || strafe != 0 || vert != 0)
                m_scene.GetCamera().FreeMove(fwd * m_free_mode_speed, strafe * m_free_mode_speed, vert * m_free_mode_speed);
        }
        if (!io->WantCaptureMouse) {
            // Right mouse button: look around
            if (glfwGetMouseButton(m_window, GLFW_MOUSE_BUTTON_RIGHT) == GLFW_PRESS)
                m_scene.GetCamera().FreeLook(x - lx, ly - y);
            // Left mouse button: still allows orbit
            else if (glfwGetMouseButton(m_window, GLFW_MOUSE_BUTTON_LEFT) == GLFW_PRESS)
                m_scene.GetCamera().ProcessInput(x - lx, ly - y, last_scroll);
            else if (last_scroll != 0)
                m_scene.GetCamera().ProcessInput(0, 0, last_scroll);
        }
    } else if (!io->WantCaptureMouse && !io->WantCaptureKeyboard) {
        if (glfwGetMouseButton(m_window, GLFW_MOUSE_BUTTON_LEFT) == GLFW_PRESS)
            m_scene.GetCamera().ProcessInput(x - lx, ly - y, last_scroll);
        else if (glfwGetMouseButton(m_window, GLFW_MOUSE_BUTTON_RIGHT) == GLFW_PRESS)
            m_scene.GetCamera().Pan(x - lx, ly - y);
        else if (last_scroll != 0)
            m_scene.GetCamera().ProcessInput(0, 0, last_scroll);
    }
    last_scroll = 0;
    lx = x; ly = y;
}

void Engine::Run()
{
    // Frame cap: when VSync is off there is no hardware throttle, so the loop
    // would run at thousands of FPS, burning CPU/GPU for no perceptible gain.
    // Capping at 240 FPS (≈4.2 ms/frame) keeps resource usage reasonable.
    using Clock = std::chrono::steady_clock;
    using Duration = std::chrono::duration<double>;
    static constexpr double kMaxFPS = 240.0;
    static constexpr double kMinFrameSeconds = 1.0 / kMaxFPS;

    auto frame_start = Clock::now();

    while (!glfwWindowShouldClose(m_window)) {
        glfwPollEvents();
        glfwGetFramebufferSize(m_window,
                               &m_scene.GetWindow().width(),
                               &m_scene.GetWindow().height());
        ProcessInput();
        Render();
        glfwSwapBuffers(m_window); // blocks here when VSync is on

        // When VSync is off, sleep any remaining time to hit the 240 FPS cap
        if (!m_settings.vsync) {
            auto now = Clock::now();
            auto elapsed = std::chrono::duration_cast<Duration>(now - frame_start).count();
            if (elapsed < kMinFrameSeconds) {
                auto sleep_dur = std::chrono::duration<double>(kMinFrameSeconds - elapsed);
                std::this_thread::sleep_for(sleep_dur);
            }
        }
        frame_start = Clock::now();
    }
}

void Engine::Shutdown() const
{
    ImGui_ImplOpenGL2_Shutdown();
    ImGui_ImplGlfw_Shutdown();
    ImGui::DestroyContext();
    glfwDestroyWindow(m_window);
    glfwTerminate();
}

void Engine::setupEnvironment()
{
    m_system_environment.glew_version   = (char*)glewGetString(GLEW_VERSION);
    m_system_environment.glfw_version   = glfwGetVersionString();
    m_system_environment.imgui_version  = IMGUI_VERSION;
    m_system_environment.opengl_version = (char*)glGetString(GL_VERSION);
    m_system_environment.gpu_renderer   = (char*)glGetString(GL_RENDERER);
}

// ============================================================================
// IMPLEMENTACOES DAS FUNCOES DE FRUSTUM CULLING
// ============================================================================

/**
 * @brief Obtém o frustum de visualização atual baseado na câmara
 * @return Frustum calculado a partir dos parâmetros da câmara
 * 
 * O frustum representa a pirâmide de visão da câmara e é utilizado
 * para testar quais os modelos devem ser renderizados (frustum culling).
 */
Frustum Engine::GetCurrentFrustum() const
{
    const Camera &camera = m_scene.GetCamera();
    const Window &window = m_scene.GetWindow();
    
    return CreateFrustumFromCamera(
        camera.position(),
        camera.looking_at(),
        camera.up(),
        camera.fov(),
        (float)window.GetWidth() / (float)window.GetHeight(),
        camera.near(),
        camera.far()
    );
}

/**
 * @brief Verifica se um AABB está dentro do frustum de visualização
 * @param aabb Caixa delimitadora a testar
 * @return true se o AABB está visivel (dentro ou intersectando o frustum)
 * 
 * Étil para evitar renderizar modelos que não estão na área de visão.
 */
bool Engine::IsAABBInFrustum(const AABB &aabb) const
{
    return GetCurrentFrustum().HasInside(aabb);
}
