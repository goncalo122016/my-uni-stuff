#ifndef ENGINE_SETTINGS_H
#define ENGINE_SETTINGS_H

class EngineSettings
{
public:
    EngineSettings(bool wireframe, bool render_axis, bool cull_faces, bool fullscreen)
        : wireframe(wireframe), render_axis(render_axis),
          cull_faces(cull_faces), fullscreen(fullscreen) {}

    bool wireframe;
    bool render_axis;
    bool cull_faces;
    bool fullscreen;
    bool lighting         = true;
    bool render_orbits    = true;
    bool frustum_culling  = true;
    bool vsync            = true;
    bool render_aabb      = false;
    bool render_normals   = false;
};

#endif // ENGINE_SETTINGS_H
