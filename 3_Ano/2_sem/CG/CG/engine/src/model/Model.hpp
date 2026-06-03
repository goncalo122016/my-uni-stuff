#ifndef MODEL_H
#define MODEL_H

#include <istream>
#include <optional>
#include <string>
#include <vector>

#include "Vec.hpp"
#include <GL/glew.h>
#include "../core/Frustum.hpp"

enum class ModelLoadFormat { OBJ, _3D, _3MF };

std::optional<ModelLoadFormat> GetModelLoadFormat(const std::string &file_path);

// Free these after all models are GPU-initialised (eager init in Engine::Init).
void ClearModelCPUCache();

struct Material
{
    float diffuse[4]  = {1.0f,  1.0f,  1.0f,  1.0f};
    float ambient[4]  = {0.2f,  0.2f,  0.2f,  1.0f};
    float specular[4] = {0.3f,  0.3f,  0.3f,  1.0f};
    float emissive[4] = {0.0f,  0.0f,  0.0f,  1.0f};
    float shininess   = 16.0f;
};

class Model
{
    std::string m_name;

    // CPU geometry (unique vertices after deduplication; cleared after GPU upload)
    std::vector<Vec3f>     m_positions;
    std::vector<Vec3f>     m_normals;
    std::vector<Vec2f>     m_texcoords;
    std::vector<uint32_t>  m_indices;    // index list for glDrawElements

    // Cached at load time — never recomputed per-frame
    AABB   m_cached_aabb;
    size_t m_vertex_count = 0; // unique vertex count
    size_t m_index_count  = 0; // index count (draw_count)

    // Per-instance data
    Material    m_material;
    std::string m_texture_path;
    bool        m_has_texture  = false;
    bool        m_skybox       = false;

    // GPU handles
    GLuint m_vbo_pos          = 0;
    GLuint m_vbo_norm         = 0;
    GLuint m_vbo_tex          = 0;
    GLuint m_vbo_idx          = 0;
    GLuint m_vbo_normals_debug = 0;  // interleaved pairs: origin, origin+normal*scale
    size_t m_normals_debug_count = 0;
    GLuint m_texture_id = 0;
    bool   m_gpu_ready = false;

    // Set when we reuse shared cached VBOs (don't delete on cleanup)
    bool m_using_shared_geo = false;
    bool m_using_shared_tex = false;

public:
    explicit Model(std::string name) : m_name(std::move(name)) {}

    const std::string &GetName()      const { return m_name; }
    size_t             VertexCount()  const { return m_vertex_count; }
    size_t             IndexCount()   const { return m_index_count; }

    bool HasTexture()   const { return m_has_texture; }
    bool HasNormals()   const { return !m_normals.empty(); }
    bool HasTexCoords() const { return !m_texcoords.empty(); }

    // AABB cached at load time — O(1), never iterates vertices at runtime
    AABB GetAABB() const { return m_cached_aabb; }

    // Legacy accessors kept for compatibility
    const std::vector<Vec3f> &GetVertex()    const { return m_positions; }
    const std::vector<Vec2f> &GetTexCoords() const { return m_texcoords; }
    std::vector<Vec2f>       &GetTexCoords()       { return m_texcoords; }

    void SetTexturePath(const std::string &path) {
        m_texture_path = path;
        m_has_texture  = !path.empty();
    }
    void SetMaterial(const Material &mat) { m_material = mat; }
    const Material &GetMaterial() const   { return m_material; }
    void SetSkybox(bool v)                { m_skybox = v; }

    void LoadFromObjStream(std::istream &file);
    void LoadFrom3dFormatStream(std::istream &file);
    void LoadFrom3mfStream(const std::string &file_path);

    void InitGPU();
    void Render();
    void Cleanup();

    // Reset per-frame VBO binding state — call after any code that changes
    // GL_ARRAY_BUFFER without going through Model::Render (e.g. orbit curves).
    static void InvalidateVBOCache();

    friend std::optional<Model> LoadModelFromFile(const std::string &, ModelLoadFormat);

private:
    void loadTexture();
};

std::optional<Model> LoadModelFromFile(const std::string &file_path);
std::optional<Model> LoadModelFromFile(const std::string &file_path, ModelLoadFormat format);

#endif // MODEL_H
