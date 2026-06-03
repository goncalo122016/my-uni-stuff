#ifndef LIGHTING_H
#define LIGHTING_H

#define GL_SILENCE_DEPRECATION
#include <GL/glew.h>
#include <vector>
#include "../scene/Scene.hpp"

namespace Lighting
{
    inline void SetupWorldLights(const std::vector<Light> &lights)
    {
        glEnable(GL_RESCALE_NORMAL);
        // Low global ambient so space is dark; sun/emissive materials supply their own light
        float amb[4] = {0.1f, 0.1f, 0.1f, 1.0f};
        glLightModelfv(GL_LIGHT_MODEL_AMBIENT, amb);
        glEnable(GL_LIGHTING);

        const int count = std::min((int)lights.size(), 8);

        // Enable active lights
        for (int i = 0; i < count; i++) {
            float white[4] = {1.0f, 1.0f, 1.0f, 1.0f};
            float small_amb[4] = {0.05f, 0.05f, 0.05f, 1.0f};
            glEnable(GL_LIGHT0 + i);
            glLightfv(GL_LIGHT0 + i, GL_AMBIENT,  small_amb);
            glLightfv(GL_LIGHT0 + i, GL_DIFFUSE,  white);
            glLightfv(GL_LIGHT0 + i, GL_SPECULAR, white);
            // Reset spotlight cutoff to default (non-spotlight) for all lights
            glLightf(GL_LIGHT0 + i, GL_SPOT_CUTOFF, 180.0f);
        }

        // Disable unused light slots
        for (int i = count; i < 8; i++)
            glDisable(GL_LIGHT0 + i);
    }

    inline void RenderLights(const std::vector<Light> &lights)
    {
        const int count = std::min((int)lights.size(), 8);
        for (int i = 0; i < count; i++) {
            std::visit([i](const auto &light) {
                using T = std::decay_t<decltype(light)>;

                if constexpr (std::is_same_v<T, DirectionalLight>) {
                    float dir[4] = {light.dir.x, light.dir.y, light.dir.z, 0.0f};
                    glLightfv(GL_LIGHT0 + i, GL_POSITION, dir);

                } else if constexpr (std::is_same_v<T, PointLight>) {
                    float pos[4] = {light.pos.x, light.pos.y, light.pos.z, 1.0f};
                    glLightfv(GL_LIGHT0 + i, GL_POSITION, pos);

                } else if constexpr (std::is_same_v<T, Spotlight>) {
                    float pos[4] = {light.pos.x, light.pos.y, light.pos.z, 1.0f};
                    float dir[4] = {light.dir.x, light.dir.y, light.dir.z, 0.0f};
                    glLightfv(GL_LIGHT0 + i, GL_POSITION,       pos);
                    glLightfv(GL_LIGHT0 + i, GL_SPOT_DIRECTION, dir);
                    glLightf (GL_LIGHT0 + i, GL_SPOT_CUTOFF,    light.cutoff);
                }
            }, lights[i]);
        }
    }

    inline void ApplyMaterial(const ModelMaterial &material)
    {
        glMaterialfv(GL_FRONT, GL_AMBIENT,   material.ambient);
        glMaterialfv(GL_FRONT, GL_DIFFUSE,   material.diffuse);
        glMaterialfv(GL_FRONT, GL_SPECULAR,  material.specular);
        glMaterialfv(GL_FRONT, GL_EMISSION,  material.emissive);
        glMaterialf (GL_FRONT, GL_SHININESS, material.shininess);
    }
}

#endif // LIGHTING_H
