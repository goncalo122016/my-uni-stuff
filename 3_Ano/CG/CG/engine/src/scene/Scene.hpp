#ifndef SCENE_H
#define SCENE_H

#include <string>
#include <vector>
#include <variant>
#include "Window.hpp"
#include "Camera.hpp"
#include "Group.hpp"
#include <Vec.hpp>

// ============================================================================
// TIPOS DE MATERIAIS
// ============================================================================

/// @brief Estrutura que define as propriedades de material de um modelo.
/// Usada para definir como um objeto interage com a iluminacao (modelo Phong).
struct ModelMaterial {
    float ambient[4]  = {0.196f, 0.196f, 0.196f, 1.0f};  // Cor refletida em sombra
    float diffuse[4]  = {0.784f, 0.784f, 0.784f, 1.0f};  // Cor refletida difusamente
    float specular[4] = {0.0f,   0.0f,   0.0f,   1.0f};  // Cor dos brilhos especulares
    float emissive[4] = {0.0f,   0.0f,   0.0f,   1.0f};  // Luz emitida pelo objeto
    float shininess   = 0.0f;                             // Expoente Phong (0-128)
};

// ============================================================================
// TIPOS DE LUZES
// ============================================================================

/// @brief Luz direcional que simula uma fonte de luz distante (como o Sol).
/// A direcao eh infinita (w=0 em OpenGL).
struct DirectionalLight {
    Vec3f dir;  // Direcao da luz (normalizada)
};

/// @brief Luz pontual que simula uma lampada em posicao especifica.
/// Emite luz em todas as direcoes a partir de um ponto (w=1 em OpenGL).
struct PointLight {
    Vec3f pos;  // Posicao da luz no espaco
};

/// @brief Projetor - luz pontual com direcao e angulo de corte (cone).
/// Combina caracteristicas de PointLight com limitacao direcional.
struct Spotlight {
    Vec3f pos;      // Posicao do projetor
    Vec3f dir;      // Direcao do cone de luz (normalizada)
    float cutoff;   // Angulo de corte em graus (0-90, ou 180 para desabilitar)
};

// Tipo uniao para armazenar qualquer tipo de luz
using Light = std::variant<DirectionalLight, PointLight, Spotlight>;

// Compatibilidade com codigo legado
enum class LightType { POINT, DIRECTIONAL, SPOT };

class Scene
{
private:
    std::string  m_name;
    Window       m_window;
    Camera       m_camera;
    Camera       m_default_camera;
    Group        m_root_group;
    std::vector<Light> m_lights;
    float m_background_color[4] = {0.0f, 0.0f, 0.0f, 1.0f};  // RGBA

public:
    explicit Scene(std::string name) : m_name(std::move(name)) {}

    const std::string &GetName()       const { return m_name; }
    Window            &GetWindow()           { return m_window; }
    const Window      &GetWindow()     const { return m_window; }
    Camera            &GetCamera()           { return m_camera; }
    const Camera      &GetCamera()     const { return m_camera; }
    void               ResetCamera()         { m_camera = m_default_camera; }
    Group             &GetRootGroup()        { return m_root_group; }
    std::vector<Light>&GetLights()           { return m_lights; }
    const float       *GetBackgroundColor()  const { return m_background_color; }
    float             *GetBackgroundColorMutable()  { return m_background_color; }

    bool LoadFromXml(const std::string &file_path);
};

#endif // SCENE_H
