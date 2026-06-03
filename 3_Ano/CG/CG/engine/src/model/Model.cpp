#include "Model.hpp"
#include "../core/Engine.hpp"

#include <filesystem>
#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <cstring>
#include <cstdlib>
#include <cstdint>
#include <unistd.h>
#include <tinyxml2.h>

#include "../utils/Utils.hpp"
#include "../stb_image.h"

// ── Search paths ──────────────────────────────────────────────────────────────

const std::vector<std::string> MODEL_PATHS_TO_SEARCH = {
    "assets/models/", "assets/test_files/", "./",
    "../../assets/models/", "../../assets/test_files/",
    "../assets/models/", "../assets/test_files/",
    "../../../assets/models/", "../../../assets/test_files/"
};

const std::vector<std::string> TEXTURE_PATHS_TO_SEARCH = {
    "assets/textures/", "./",
    "../../assets/textures/", "../assets/textures/",
    "../../../assets/textures/"
};

// ── Static GPU caches ─────────────────────────────────────────────────────────
// One entry per unique model file and texture file.
// All Model instances referencing the same path share VBOs / texture IDs,
// so the GPU data is uploaded exactly once regardless of how many groups
// reference the same file in the XML scene.

struct CachedGeo {
    GLuint vbo_pos    = 0;
    GLuint vbo_norm   = 0;
    GLuint vbo_tex    = 0;
    GLuint vbo_idx    = 0;
    GLuint vbo_normals_debug  = 0;
    size_t normals_debug_count = 0;
    size_t index_count  = 0;
    size_t vertex_count = 0;
    AABB   aabb;
};

static std::unordered_map<std::string, CachedGeo> s_geo_cache;
static std::unordered_map<std::string, GLuint>    s_tex_cache;

// Per-frame render-state caches — avoid redundant GPU state changes
static GLuint  s_bound_texture = UINT32_MAX; // current glBindTexture target
static Material s_last_mat;                  // last glMaterialfv state
static bool     s_mat_valid    = false;      // whether s_last_mat is set

// Per-frame VBO binding caches — avoid redundant glBindBuffer+gl*Pointer calls
// when consecutive models share the same GPU buffers (common in solar system where
// every planet uses the same sphere.3d VBOs).
static GLuint s_bound_vbo_pos  = 0;
static GLuint s_bound_vbo_norm = 0;
static GLuint s_bound_vbo_tex  = 0;
static GLuint s_bound_vbo_idx  = 0;

// ── CPU geometry cache ────────────────────────────────────────────────────────
// Stores parsed vertex data indexed by canonical file path.
// LoadModelFromFile populates this on the FIRST load of a given file and moves
// the vectors here; subsequent loads for the same path return a shell Model with
// no CPU vectors. InitGPU() reads from here when uploading to the GPU.
// Call ClearModelCPUCache() after all models are GPU-initialised to free RAM.
struct CPUGeo {
    std::vector<Vec3f>    positions;
    std::vector<Vec3f>    normals;
    std::vector<Vec2f>    texcoords;
    std::vector<uint32_t> indices;
    AABB                  aabb;
    size_t                vertex_count = 0;
    size_t                index_count  = 0;
};
static std::unordered_map<std::string, CPUGeo> s_cpu_geo_cache;

void ClearModelCPUCache() { s_cpu_geo_cache.clear(); }

// ── Vertex deduplication helpers ──────────────────────────────────────────────

// Key used to detect duplicate vertices in the .3d flat-list format.
// memcmp equality, FNV-inspired hash.
struct VertexKey {
    float px, py, pz;
    float nx, ny, nz;
    float u,  v;
};

struct VertexKeyHash {
    size_t operator()(const VertexKey &k) const noexcept {
        const uint32_t *d = reinterpret_cast<const uint32_t *>(&k);
        size_t h = 14695981039346656037ull;
        for (size_t i = 0; i < sizeof(VertexKey) / 4; ++i)
            h = (h ^ d[i]) * 1099511628211ull;
        return h;
    }
};

struct VertexKeyEq {
    bool operator()(const VertexKey &a, const VertexKey &b) const noexcept {
        return std::memcmp(&a, &b, sizeof(VertexKey)) == 0;
    }
};

// Key used to deduplicate OBJ face vertices (v / vt / vn triplet).
struct ObjVKey { int v, vt, vn; };

struct ObjVKeyHash {
    size_t operator()(const ObjVKey &k) const noexcept {
        size_t h = (size_t)k.v * 2654435761ull;
        h ^= (size_t)k.vt * 2246822519ull;
        h ^= (size_t)k.vn * 3266489917ull;
        return h;
    }
};

struct ObjVKeyEq {
    bool operator()(const ObjVKey &a, const ObjVKey &b) const noexcept {
        return a.v == b.v && a.vt == b.vt && a.vn == b.vn;
    }
};

// ── .3d Loader ────────────────────────────────────────────────────────────────
// Flat vertex list: N lines of "x y z [nx ny nz u v]"
// Deduplicates vertices → generates index buffer.
// Caches AABB and vertex count during loading.
void Model::LoadFrom3dFormatStream(std::istream &file)
{
    std::string line;
    std::getline(file, line);
    size_t count = std::stoul(line);

    std::unordered_map<VertexKey, uint32_t, VertexKeyHash, VertexKeyEq> vmap;
    vmap.reserve(count);
    m_positions.reserve(count / 2);
    m_normals.reserve(count / 2);
    m_texcoords.reserve(count / 2);
    m_indices.reserve(count);

    for (size_t i = 0; i < count; ++i) {
        std::getline(file, line);
        std::istringstream iss(line);

        Vec3f pos, norm{};
        float u = 0, v = 0;
        iss >> pos.x >> pos.y >> pos.z;
        iss >> norm.x >> norm.y >> norm.z >> u >> v;

        m_cached_aabb.Extend(pos);

        VertexKey key{pos.x, pos.y, pos.z, norm.x, norm.y, norm.z, u, v};
        auto it = vmap.find(key);
        if (it != vmap.end()) {
            m_indices.push_back(it->second);
        } else {
            uint32_t idx = (uint32_t)m_positions.size();
            m_positions.push_back(pos);
            m_normals.push_back(norm);
            m_texcoords.push_back(Vec2f(u, v));
            vmap[key] = idx;
            m_indices.push_back(idx);
        }
    }

    m_vertex_count = m_positions.size();
    m_index_count  = m_indices.size();
}

// ── OBJ Loader ────────────────────────────────────────────────────────────────
// Properly handles indexed OBJ (v/vt/vn per face vertex).
// Deduplicates by (v, vt, vn) triplet.
static int parseObjFaceToken(const std::string &tok, int &vt_out, int &vn_out)
{
    int v = -1; vt_out = -1; vn_out = -1;
    std::istringstream ss(tok);
    std::string part;
    if (std::getline(ss, part, '/') && !part.empty()) v = std::stoi(part) - 1;
    if (std::getline(ss, part, '/') && !part.empty()) vt_out = std::stoi(part) - 1;
    if (std::getline(ss, part, '/') && !part.empty()) vn_out = std::stoi(part) - 1;
    return v;
}

void Model::LoadFromObjStream(std::istream &file)
{
    std::vector<Vec3f> tmp_v;
    std::vector<std::pair<float,float>> tmp_vt;
    std::vector<Vec3f> tmp_vn;

    std::unordered_map<ObjVKey, uint32_t, ObjVKeyHash, ObjVKeyEq> vmap;
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty() || line[0] == '#') continue;
        std::istringstream ss(line);
        std::string type; ss >> type;

        if (type == "v") {
            Vec3f v; ss >> v.x >> v.y >> v.z;
            tmp_v.push_back(v);
        } else if (type == "vt") {
            float u, vv; ss >> u >> vv;
            tmp_vt.push_back({u, vv});
        } else if (type == "vn") {
            Vec3f vn; ss >> vn.x >> vn.y >> vn.z;
            tmp_vn.push_back(vn);
        } else if (type == "f") {
            // Fan-triangulate the face (handles quads and polygons)
            std::vector<std::string> tokens;
            std::string tok;
            while (ss >> tok) tokens.push_back(tok);

            for (size_t t = 1; t + 1 < tokens.size(); ++t) {
                std::string corners[3] = {tokens[0], tokens[t], tokens[t+1]};
                for (auto &c : corners) {
                    int vt, vn;
                    int vi = parseObjFaceToken(c, vt, vn);
                    ObjVKey key{vi, vt, vn};
                    auto it = vmap.find(key);
                    if (it != vmap.end()) {
                        m_indices.push_back(it->second);
                    } else {
                        uint32_t idx = (uint32_t)m_positions.size();
                        Vec3f pos = (vi >= 0 && vi < (int)tmp_v.size()) ? tmp_v[vi] : Vec3f{};
                        m_positions.push_back(pos);
                        m_cached_aabb.Extend(pos);
                        if (vt >= 0 && vt < (int)tmp_vt.size())
                            m_texcoords.push_back(Vec2f(tmp_vt[vt].first, tmp_vt[vt].second));
                        else
                            m_texcoords.push_back(Vec2f(0,0));
                        if (vn >= 0 && vn < (int)tmp_vn.size())
                            m_normals.push_back(tmp_vn[vn]);
                        else
                            m_normals.push_back(Vec3f{0,1,0});
                        vmap[key] = idx;
                        m_indices.push_back(idx);
                    }
                }
            }
        }
    }

    m_vertex_count = m_positions.size();
    m_index_count  = m_indices.size();
}

// ── .3MF Loader ───────────────────────────────────────────────────────────────
static std::string Extract3mfModel(const std::string &zip_path)
{
    std::string temp_file = "/tmp/3dmodel_temp_" + std::to_string(getpid()) + ".model";
    std::string cmd = "unzip -p \"" + zip_path + "\" \"3D/3dmodel.model\" > \"" + temp_file + "\" 2>/dev/null";
    if (system(cmd.c_str()) != 0) return "";
    std::ifstream f(temp_file);
    if (!f.is_open()) return "";
    std::stringstream buf; buf << f.rdbuf();
    std::remove(temp_file.c_str());
    return buf.str();
}

void Model::LoadFrom3mfStream(const std::string &file_path)
{
    std::string xml_content = Extract3mfModel(file_path);
    if (xml_content.empty()) return;

    tinyxml2::XMLDocument doc;
    if (doc.Parse(xml_content.c_str()) != tinyxml2::XML_SUCCESS) return;

    auto *model_elem = doc.FirstChildElement("model");
    if (!model_elem) return;
    auto *resources = model_elem->FirstChildElement("resources");
    if (!resources) return;

    for (auto *obj = resources->FirstChildElement("object"); obj;
         obj = obj->NextSiblingElement("object")) {
        const char *type = obj->Attribute("type");
        if (!type || std::string(type) != "model") continue;
        auto *mesh = obj->FirstChildElement("mesh");
        if (!mesh) continue;

        std::vector<Vec3f> vertices;
        if (auto *ve = mesh->FirstChildElement("vertices"))
            for (auto *v = ve->FirstChildElement("vertex"); v; v = v->NextSiblingElement("vertex")) {
                float x=0,y=0,z=0;
                v->QueryFloatAttribute("x",&x); v->QueryFloatAttribute("y",&y); v->QueryFloatAttribute("z",&z);
                vertices.push_back({x,y,z});
            }

        if (auto *te = mesh->FirstChildElement("triangles"))
            for (auto *tri = te->FirstChildElement("triangle"); tri; tri = tri->NextSiblingElement("triangle")) {
                int v1=0,v2=0,v3=0;
                tri->QueryIntAttribute("v1",&v1); tri->QueryIntAttribute("v2",&v2); tri->QueryIntAttribute("v3",&v3);
                if (v1<(int)vertices.size() && v2<(int)vertices.size() && v3<(int)vertices.size()) {
                    for (int vi : {v1,v2,v3}) {
                        uint32_t idx = (uint32_t)m_positions.size();
                        m_positions.push_back(vertices[vi]);
                        m_cached_aabb.Extend(vertices[vi]);
                        m_indices.push_back(idx);
                    }
                }
            }
        break;
    }

    m_vertex_count = m_positions.size();
    m_index_count  = m_indices.size();
}

// ── Texture loading ───────────────────────────────────────────────────────────
void Model::loadTexture()
{
    if (m_texture_path.empty()) return;

    // Texture cache hit: reuse GPU texture ID
    auto tcit = s_tex_cache.find(m_texture_path);
    if (tcit != s_tex_cache.end()) {
        m_texture_id       = tcit->second;
        m_using_shared_tex = true;
        return;
    }

    std::string full_path = m_texture_path;
    if (!std::filesystem::exists(full_path)) {
        auto found = utils::FindFile(TEXTURE_PATHS_TO_SEARCH, m_texture_path);
        if (!found) {
            std::cerr << "Warning: texture not found: " << m_texture_path << "\n";
            m_has_texture = false;
            return;
        }
        full_path = found->string();
    }

    stbi_set_flip_vertically_on_load(true);
    int w, h, ch;
    unsigned char *data = stbi_load(full_path.c_str(), &w, &h, &ch, 0);
    if (!data) {
        std::cerr << "Warning: failed to load texture: " << full_path << "\n";
        m_has_texture = false;
        return;
    }

    glGenTextures(1, &m_texture_id);
    glBindTexture(GL_TEXTURE_2D, m_texture_id);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);

    GLenum fmt = (ch == 4) ? GL_RGBA : GL_RGB;
    glTexImage2D(GL_TEXTURE_2D, 0, fmt, w, h, 0, fmt, GL_UNSIGNED_BYTE, data);
    glGenerateMipmap(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, 0);
    stbi_image_free(data);

    s_tex_cache[m_texture_path] = m_texture_id;
}

// ── GPU initialisation ────────────────────────────────────────────────────────
// Called lazily on first Render().
// Checks static caches first — if geometry for this file already lives on GPU,
// we reuse the existing VBOs instead of uploading again.
void Model::InitGPU()
{
    if (m_gpu_ready) return;

    // ── Geometry cache ──────────────────────────────────────────────────────
    auto gcit = s_geo_cache.find(m_name);
    if (gcit != s_geo_cache.end()) {
        // Reuse shared VBOs — no re-upload needed
        const CachedGeo &g = gcit->second;
        m_vbo_pos             = g.vbo_pos;
        m_vbo_norm            = g.vbo_norm;
        m_vbo_tex             = g.vbo_tex;
        m_vbo_idx             = g.vbo_idx;
        m_vbo_normals_debug   = g.vbo_normals_debug;
        m_normals_debug_count = g.normals_debug_count;
        m_index_count         = g.index_count;
        m_vertex_count        = g.vertex_count;
        m_cached_aabb         = g.aabb;
        m_using_shared_geo    = true;
    } else {
        // First GPU upload for this file path.
        // Geometry lives in s_cpu_geo_cache (moved there by LoadModelFromFile).
        auto cpu_it = s_cpu_geo_cache.find(m_name);
        if (cpu_it == s_cpu_geo_cache.end()) {
            std::cerr << "[Model] InitGPU: no CPU geometry for '" << m_name << "'\n";
            m_gpu_ready = true;
            return;
        }
        const CPUGeo &cpu = cpu_it->second;

        glGenBuffers(1, &m_vbo_pos);
        glBindBuffer(GL_ARRAY_BUFFER, m_vbo_pos);
        glBufferData(GL_ARRAY_BUFFER,
                     cpu.positions.size() * sizeof(Vec3f),
                     cpu.positions.data(), GL_STATIC_DRAW);

        if (!cpu.normals.empty()) {
            glGenBuffers(1, &m_vbo_norm);
            glBindBuffer(GL_ARRAY_BUFFER, m_vbo_norm);
            glBufferData(GL_ARRAY_BUFFER,
                         cpu.normals.size() * sizeof(Vec3f),
                         cpu.normals.data(), GL_STATIC_DRAW);
        }

        if (!cpu.texcoords.empty()) {
            glGenBuffers(1, &m_vbo_tex);
            glBindBuffer(GL_ARRAY_BUFFER, m_vbo_tex);
            glBufferData(GL_ARRAY_BUFFER,
                         cpu.texcoords.size() * sizeof(Vec2f),
                         cpu.texcoords.data(), GL_STATIC_DRAW);
        }

        if (!cpu.indices.empty()) {
            glGenBuffers(1, &m_vbo_idx);
            glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_vbo_idx);
            glBufferData(GL_ELEMENT_ARRAY_BUFFER,
                         cpu.indices.size() * sizeof(uint32_t),
                         cpu.indices.data(), GL_STATIC_DRAW);
        }

        // Normals debug VBO: pairs (origin, origin + normal * scale) for GL_LINES
        if (!cpu.positions.empty() && !cpu.normals.empty()) {
            constexpr float NORMAL_SCALE = 0.1f;
            std::vector<Vec3f> lines;
            lines.reserve(cpu.positions.size() * 2);
            for (size_t i = 0; i < cpu.positions.size(); ++i) {
                lines.push_back(cpu.positions[i]);
                lines.push_back(cpu.positions[i] + cpu.normals[i] * NORMAL_SCALE);
            }
            glGenBuffers(1, &m_vbo_normals_debug);
            glBindBuffer(GL_ARRAY_BUFFER, m_vbo_normals_debug);
            glBufferData(GL_ARRAY_BUFFER,
                         lines.size() * sizeof(Vec3f),
                         lines.data(), GL_STATIC_DRAW);
            m_normals_debug_count = lines.size();
        }

        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

        // Mirror cached counts (shell models have these set; first model also needs them)
        m_vertex_count = cpu.vertex_count;
        m_index_count  = cpu.index_count;
        m_cached_aabb  = cpu.aabb;

        // Store GPU handles in cache so other shell models reuse them
        CachedGeo g;
        g.vbo_pos             = m_vbo_pos;
        g.vbo_norm            = m_vbo_norm;
        g.vbo_tex             = m_vbo_tex;
        g.vbo_idx             = m_vbo_idx;
        g.vbo_normals_debug   = m_vbo_normals_debug;
        g.normals_debug_count = m_normals_debug_count;
        g.index_count         = m_index_count;
        g.vertex_count        = m_vertex_count;
        g.aabb                = m_cached_aabb;
        s_geo_cache[m_name] = g;
        // CPU vectors are empty (moved to s_cpu_geo_cache); nothing to free here.
        // Call ClearModelCPUCache() from Engine::Init after all initAllGPU() calls.
    }

    // ── Texture ─────────────────────────────────────────────────────────────
    if (m_has_texture) loadTexture();

    m_gpu_ready = true;
}

// ── Render-state invalidation ─────────────────────────────────────────────────
// Call this after any code that mutates GL_ARRAY_BUFFER / glVertexPointer outside
// of Model::Render (e.g. orbit curve / orbit circle VBO draws in Group.cpp).
void Model::InvalidateVBOCache()
{
    s_bound_vbo_pos = s_bound_vbo_norm = s_bound_vbo_tex = s_bound_vbo_idx = 0;
    // Orbit curves call glColor3f which corrupts the unlit color — force the next
    // model to re-upload its material (which includes glColor4f).
    s_mat_valid = false;
}

// ── Render ────────────────────────────────────────────────────────────────────
// Client states (GL_VERTEX_ARRAY, GL_NORMAL_ARRAY, GL_TEXTURE_COORD_ARRAY)
// and GL_TEXTURE_2D are enabled once in Engine::Init() and stay enabled.
// We only swap buffer bindings and texture bindings here.
void Model::Render()
{
    if (!m_gpu_ready) InitGPU();

    // Skybox: disable lighting and depth writes
    if (m_skybox) {
        glPushAttrib(GL_ENABLE_BIT | GL_DEPTH_BUFFER_BIT);
        glDisable(GL_LIGHTING);
        glDisable(GL_CULL_FACE);
        glDepthMask(GL_FALSE);
    }

    // Material + color — only upload to GPU when it actually changed.
    // InvalidateVBOCache() sets s_mat_valid=false after orbit curve rendering
    // (which calls glColor3f and corrupts the unlit color), so the next model
    // always re-uploads its material including glColor4f.
    if (!s_mat_valid || std::memcmp(&m_material, &s_last_mat, sizeof(Material)) != 0) {
        glColor4f(m_material.diffuse[0], m_material.diffuse[1],
                  m_material.diffuse[2], m_material.diffuse[3]);
        glMaterialfv(GL_FRONT, GL_DIFFUSE,   m_material.diffuse);
        glMaterialfv(GL_FRONT, GL_AMBIENT,   m_material.ambient);
        glMaterialfv(GL_FRONT, GL_SPECULAR,  m_material.specular);
        glMaterialfv(GL_FRONT, GL_EMISSION,  m_material.emissive);
        glMaterialf (GL_FRONT, GL_SHININESS, m_material.shininess);
        s_last_mat  = m_material;
        s_mat_valid = true;
    }

    // Texture — only rebind when the texture ID changes
    GLuint tex_to_bind = (m_has_texture && m_texture_id) ? m_texture_id : 0u;
    if (tex_to_bind != s_bound_texture) {
        glBindTexture(GL_TEXTURE_2D, tex_to_bind);
        s_bound_texture = tex_to_bind;
    }

    // Vertex positions — skip if same VBO already bound (solar system: all
    // planets share sphere.3d VBOs, so this saves ~5 GPU state calls per planet)
    if (m_vbo_pos != s_bound_vbo_pos) {
        glBindBuffer(GL_ARRAY_BUFFER, m_vbo_pos);
        glVertexPointer(3, GL_FLOAT, 0, nullptr);
        s_bound_vbo_pos = m_vbo_pos;
    }

    if (m_vbo_norm && m_vbo_norm != s_bound_vbo_norm) {
        glBindBuffer(GL_ARRAY_BUFFER, m_vbo_norm);
        glNormalPointer(GL_FLOAT, 0, nullptr);
        s_bound_vbo_norm = m_vbo_norm;
    }

    if (m_vbo_tex && m_vbo_tex != s_bound_vbo_tex) {
        glBindBuffer(GL_ARRAY_BUFFER, m_vbo_tex);
        glTexCoordPointer(2, GL_FLOAT, 0, nullptr);
        s_bound_vbo_tex = m_vbo_tex;
    }

    // Draw — indexed if available, otherwise flat array
    if (m_vbo_idx) {
        if (m_vbo_idx != s_bound_vbo_idx) {
            glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_vbo_idx);
            s_bound_vbo_idx = m_vbo_idx;
        }
        glDrawElements(GL_TRIANGLES, (GLsizei)m_index_count, GL_UNSIGNED_INT, nullptr);
    } else {
        glDrawArrays(GL_TRIANGLES, 0, (GLsizei)m_vertex_count);
    }

    // Normal visualisation — draw only when the debug flag is active
    if (m_vbo_normals_debug &&
        Engine::g_engine_instance && Engine::g_engine_instance->IsRenderNormals()) {
        glDisable(GL_LIGHTING);
        glDisableClientState(GL_NORMAL_ARRAY);
        glDisableClientState(GL_TEXTURE_COORD_ARRAY);
        glBindBuffer(GL_ARRAY_BUFFER, m_vbo_normals_debug);
        glVertexPointer(3, GL_FLOAT, 0, nullptr);
        glColor3f(0.0f, 1.0f, 0.0f);
        glDrawArrays(GL_LINES, 0, (GLsizei)m_normals_debug_count);
        glEnableClientState(GL_TEXTURE_COORD_ARRAY);
        glEnableClientState(GL_NORMAL_ARRAY);
        glEnable(GL_LIGHTING);
        // Changing the VBO and color outside normal flow — invalidate all state caches
        InvalidateVBOCache();
    }

    if (m_skybox) {
        glPopAttrib();
        // glPopAttrib restores GL_ENABLE_BIT but does NOT restore texture/material/VBO
        // state — invalidate all caches so the next model re-uploads from scratch.
        s_bound_texture = UINT32_MAX;
        s_mat_valid     = false;
        s_bound_vbo_pos = s_bound_vbo_norm = s_bound_vbo_tex = s_bound_vbo_idx = 0;
    }
}

// ── Cleanup ───────────────────────────────────────────────────────────────────
void Model::Cleanup()
{
    if (!m_gpu_ready) return;

    // Only delete VBOs/textures we own — shared ones are kept in the cache
    if (!m_using_shared_geo) {
        if (m_vbo_pos)           glDeleteBuffers(1, &m_vbo_pos);
        if (m_vbo_norm)          glDeleteBuffers(1, &m_vbo_norm);
        if (m_vbo_tex)           glDeleteBuffers(1, &m_vbo_tex);
        if (m_vbo_idx)           glDeleteBuffers(1, &m_vbo_idx);
        if (m_vbo_normals_debug) glDeleteBuffers(1, &m_vbo_normals_debug);
    }
    if (!m_using_shared_tex && m_texture_id)
        glDeleteTextures(1, &m_texture_id);

    m_vbo_pos = m_vbo_norm = m_vbo_tex = m_vbo_idx = m_vbo_normals_debug = 0;
    m_normals_debug_count = 0;
    m_texture_id = 0;
    m_gpu_ready = false;
}

// ── File format detection and loading ─────────────────────────────────────────

std::optional<ModelLoadFormat> GetModelLoadFormat(const std::string &p)
{
    if (p.ends_with(".obj") || p.ends_with(".obj.bin")) return ModelLoadFormat::OBJ;
    if (p.ends_with(".3d"))  return ModelLoadFormat::_3D;
    if (p.ends_with(".3mf")) return ModelLoadFormat::_3MF;
    return std::nullopt;
}

std::optional<Model> LoadModelFromFile(const std::string &file_path)
{
    if (const auto fmt = GetModelLoadFormat(file_path))
        return LoadModelFromFile(file_path, fmt.value());
    return std::nullopt;
}

std::optional<Model> LoadModelFromFile(const std::string &file_path, const ModelLoadFormat format)
{
    std::optional<std::filesystem::path> path;
    if (std::filesystem::exists(file_path))
        path = file_path;
    else
        path = utils::FindFile(MODEL_PATHS_TO_SEARCH, file_path);

    if (!path) return std::nullopt;

    const std::string canonical = path.value().string();

    // CPU geometry cache hit — same file was already parsed; return a shell Model.
    // InitGPU() will reuse the shared GPU buffers from s_geo_cache (no re-upload).
    auto cpu_it = s_cpu_geo_cache.find(canonical);
    if (cpu_it != s_cpu_geo_cache.end()) {
        Model shell(canonical);
        shell.m_cached_aabb  = cpu_it->second.aabb;
        shell.m_vertex_count = cpu_it->second.vertex_count;
        shell.m_index_count  = cpu_it->second.index_count;
        return shell;
    }

    // First load: parse the file into a temporary Model, then move its vectors
    // into s_cpu_geo_cache so every subsequent reference to this file gets a shell.
    std::ifstream file(canonical);
    if (!file.is_open()) return std::nullopt;

    Model model(canonical);
    switch (format) {
        case ModelLoadFormat::OBJ:  model.LoadFromObjStream(file);    break;
        case ModelLoadFormat::_3D:  model.LoadFrom3dFormatStream(file); break;
        case ModelLoadFormat::_3MF: model.LoadFrom3mfStream(canonical); break;
    }

    // Move parsed vectors to CPU cache (model becomes a shell, but retains AABB/counts)
    CPUGeo &cpu = s_cpu_geo_cache[canonical];
    cpu.positions    = std::move(model.m_positions);
    cpu.normals      = std::move(model.m_normals);
    cpu.texcoords    = std::move(model.m_texcoords);
    cpu.indices      = std::move(model.m_indices);
    cpu.aabb         = model.m_cached_aabb;
    cpu.vertex_count = model.m_vertex_count;
    cpu.index_count  = model.m_index_count;

    return model; // shell: AABB/counts set, CPU vectors empty (in cache)
}
