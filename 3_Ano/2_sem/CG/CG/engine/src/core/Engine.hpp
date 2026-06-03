#ifndef ENGINE_H
#define ENGINE_H
#define GLFW_INCLUDE_GLU
#define GL_SILENCE_DEPRECATION
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <imgui.h>
#include "../core/Frustum.hpp"
#include "../scene/Scene.hpp"
#include "EngineSystemEnvironment.hpp"
#include "EngineSettings.hpp"
#include "Lighting.hpp"

class Engine
{
public:
    explicit Engine(Scene scene) : m_scene(std::move(scene)) {}
    
    static Engine* g_engine_instance;

    bool Init();
    void Render();
    void SetWireframe(bool enable);
    void SetCullFaces(bool enable);
    void SetFullscreen(bool enable);
    void SetVsync(bool enable);
    void ProcessInput();
    void Run();
    void Shutdown() const;
    
    bool IsAnimationPaused() const { return m_animation_paused; }
    double GetPausedTime() const { return m_paused_time; }
    bool IsRenderOrbits()   const { return m_settings.render_orbits; }
    bool IsRenderAABB()     const { return m_settings.render_aabb; }
    bool IsRenderNormals()  const { return m_settings.render_normals; }

    // Frustum Culling - obtém o frustum atual da câmara
    Frustum GetCurrentFrustum() const;

    // Frustum Culling - verifica se um AABB está no frustum
    bool IsAABBInFrustum(const AABB &aabb) const;

    // Acesso ao frustum cacheado do frame atual (para uso em Group::render)
    const Frustum& GetCachedFrustum() const { return m_cached_frustum; }
    bool FrustumCullingEnabled()      const { return m_settings.frustum_culling; }

private:
    Scene m_scene;

    EngineSettings m_settings{
        false, // wireframe
        false, // render_axis
        true,  // cull_faces
        false, // fullscreen
    };

    EngineSystemEnvironment m_system_environment;
    ImGuiIO *io = nullptr;

    GLFWwindow *m_window = nullptr;
    
    // Animation pause state
    bool m_animation_paused = false;
    double m_paused_time = 0.0;
    // When paused, world positions don't change — skip updateWorldPositions after first
    // computation. Reset to false whenever animation resumes.
    bool m_positions_valid = false;

    // Free mode camera
    bool m_free_mode = false;
    float m_free_mode_speed = 0.15f;

    // Group follow
    Group* m_followed_group = nullptr;
    Vec3f  m_followed_prev_pos = {};

    // Variables to store windowed mode position and size
    int m_windowed_xpos = 0;
    int m_windowed_ypos = 0;
    int m_windowed_width = 800;
    int m_windowed_height = 600;

    // Frustum cacheado para o frame atual (evita recalcular por grupo)
    Frustum m_cached_frustum;

    // ========================================================================
    // FUNCOES DE ILUMINACAO (Sistema de Luzes - 3 tipos)
    // ========================================================================
    
    /// @brief Inicializa o sistema de luzes OpenGL durante Init()
    /// Desabilita todos os slots de luz e configura parametros globais
    void setupWorldLights();
    
    /// @brief Renderiza todas as luzes da cena (chamado a cada frame em Render())
    /// Atualiza posicoes e configuracoes das luzes
    void renderLights();
    
    /// @brief Aplica propriedades de material a um modelo (modelo Phong OpenGL)
    /// @param material Estrutura com ambient, diffuse, specular, emissive, shininess
    void applyMaterial(const ModelMaterial &material);

    // ImGui functions
    void setupEnvironment();
    void initImGui();
    void renderImGui();
    static void postRenderImGui();
};

#endif // ENGINE_H
