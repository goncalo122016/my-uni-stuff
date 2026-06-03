#include "Generator.hpp"

#define _USE_MATH_DEFINES

#include <math.h>
#include <array>
#include <fstream>
#include <sstream>
#include <string>
#include <iostream>

std::vector<Vertex> generator::GeneratePlane(const float length, const size_t divisions)
{
    std::vector<Vertex> verts;
    const float side = length / divisions;
    const Vec3f normal = {0.0f, 1.0f, 0.0f};

    for (size_t x = 0; x < divisions; ++x)
    {
        for (size_t z = 0; z < divisions; ++z)
        {
            Vec3f tl = {-length / 2 + x * side,       0, -length / 2 + z * side};
            Vec3f tr = {-length / 2 + (x+1) * side,   0, -length / 2 + z * side};
            Vec3f bl = {-length / 2 + x * side,       0, -length / 2 + (z+1) * side};
            Vec3f br = {-length / 2 + (x+1) * side,   0, -length / 2 + (z+1) * side};

            float tlu = (float)x / divisions,       tlv = (float)z / divisions;
            float tru = (float)(x+1) / divisions,   trv = (float)z / divisions;
            float blu = (float)x / divisions,       blv = (float)(z+1) / divisions;
            float bru = (float)(x+1) / divisions,   brv = (float)(z+1) / divisions;

            verts.push_back({tl, normal, tlu, tlv});
            verts.push_back({bl, normal, blu, blv});
            verts.push_back({br, normal, bru, brv});

            verts.push_back({tl, normal, tlu, tlv});
            verts.push_back({br, normal, bru, brv});
            verts.push_back({tr, normal, tru, trv});
        }
    }
    return verts;
}

std::vector<Vertex> generator::GenerateSphere(const float radius, const size_t slices, const size_t stacks)
{
    std::vector<Vertex> verts;

    const float slice_size = 2.0f * (float)M_PI / slices;
    const float stack_size = (float)M_PI / stacks;

    auto makeVert = [&](int s, int st) -> Vertex {
        float alpha = s * slice_size;
        float beta  = st * stack_size - (float)M_PI_2;
        Vec3f pos   = Vec3fSpherical(radius, alpha, beta);
        Vec3f norm  = pos.Normalized();
        float u     = (float)s / slices;
        float v     = (float)st / stacks;
        return {pos, norm, u, v};
    };

    for (int slice = 0; slice < (int)slices; ++slice)
    {
        for (int stack = 0; stack < (int)stacks; ++stack)
        {
            Vertex bl = makeVert(slice,   stack);
            Vertex br = makeVert(slice+1, stack);
            Vertex tl = makeVert(slice,   stack+1);
            Vertex tr = makeVert(slice+1, stack+1);

            if (stack != 0)
                verts.insert(verts.end(), {tl, bl, br});
            if (stack != (int)stacks - 1)
                verts.insert(verts.end(), {tl, br, tr});
        }
    }
    return verts;
}

std::vector<Vertex> generator::GenerateCone(const float radius, const float height,
                                             const size_t slices, const size_t stacks)
{
    std::vector<Vertex> verts;

    const float slice_size = 2.0f * (float)M_PI / slices;
    const float stack_size = height / stacks;
    const float slant      = sqrtf(radius * radius + height * height);

    auto lateralNorm = [&](float alpha) -> Vec3f {
        return {height * sinf(alpha) / slant, radius / slant, height * cosf(alpha) / slant};
    };

    for (int slice = 0; slice < (int)slices; ++slice)
    {
        float a1 = slice * slice_size;
        float a2 = (slice + 1) * slice_size;

        for (int stack = 0; stack < (int)stacks; ++stack)
        {
            float r_bot = radius - stack * radius / stacks;
            float r_top = radius - (stack + 1) * radius / stacks;
            float y_bot = stack * stack_size;
            float y_top = (stack + 1) * stack_size;

            Vec3f bl = Vec3fPolar(r_bot, a1, y_bot);
            Vec3f br = Vec3fPolar(r_bot, a2, y_bot);
            Vec3f tl = Vec3fPolar(r_top, a1, y_top);
            Vec3f tr = Vec3fPolar(r_top, a2, y_top);

            float u1 = (float)slice / slices,     v_bot = y_bot / height;
            float u2 = (float)(slice+1) / slices, v_top = y_top / height;

            verts.push_back({tl, lateralNorm(a1), u1, v_top});
            verts.push_back({bl, lateralNorm(a1), u1, v_bot});
            verts.push_back({br, lateralNorm(a2), u2, v_bot});

            if (stack != (int)stacks - 1) {
                verts.push_back({tl, lateralNorm(a1), u1, v_top});
                verts.push_back({br, lateralNorm(a2), u2, v_bot});
                verts.push_back({tr, lateralNorm(a2), u2, v_top});
            }
        }

        // Base disk
        Vec3f base_norm = {0.0f, -1.0f, 0.0f};
        Vec3f center    = {0.0f, 0.0f, 0.0f};
        Vec3f base_l    = Vec3fPolar(radius, a1, 0.0f);
        Vec3f base_r    = Vec3fPolar(radius, a2, 0.0f);

        float cu = 0.5f, cv = 0.5f;
        float lu = 0.5f + 0.5f * sinf(a1), lv = 0.5f + 0.5f * cosf(a1);
        float ru = 0.5f + 0.5f * sinf(a2), rv = 0.5f + 0.5f * cosf(a2);

        verts.push_back({center, base_norm, cu, cv});
        verts.push_back({base_r, base_norm, ru, rv});
        verts.push_back({base_l, base_norm, lu, lv});
    }
    return verts;
}

std::vector<Vertex> generator::GenerateBox(const float length, const size_t divisions)
{
    std::vector<Vertex> verts;
    const float side  = length / divisions;
    const float half  = length / 2.0f;

    struct FaceDef {
        Vec3f normal;
        // right and up vectors for uv
        Vec3f right;
        Vec3f up;
        Vec3f origin; // corner of the face
    };

    FaceDef faces[6] = {
        { {0,  1, 0}, { 1, 0, 0}, {0, 0,  1}, {-half, half, -half} }, // top
        { {0, -1, 0}, { 1, 0, 0}, {0, 0, -1}, {-half,-half,  half} }, // bottom
        { {-1, 0, 0}, {0, 0,  1}, {0, 1,  0}, {-half,-half, -half} }, // left
        { { 1, 0, 0}, {0, 0, -1}, {0, 1,  0}, { half,-half,  half} }, // right
        { {0, 0,  1}, { 1, 0, 0}, {0, 1,  0}, {-half,-half,  half} }, // front
        { {0, 0, -1}, {-1, 0, 0}, {0, 1,  0}, { half,-half, -half} }, // back
    };

    for (auto& f : faces)
    {
        for (size_t i = 0; i < divisions; ++i)
        {
            for (size_t j = 0; j < divisions; ++j)
            {
                Vec3f tl = f.origin + f.right * (i * side)       + f.up * (j * side);
                Vec3f tr = f.origin + f.right * ((i+1) * side)   + f.up * (j * side);
                Vec3f bl = f.origin + f.right * (i * side)       + f.up * ((j+1) * side);
                Vec3f br = f.origin + f.right * ((i+1) * side)   + f.up * ((j+1) * side);

                float tlu = (float)i / divisions,       tlv = (float)j / divisions;
                float tru = (float)(i+1) / divisions,   trv = (float)j / divisions;
                float blu = (float)i / divisions,       blv = (float)(j+1) / divisions;
                float bru = (float)(i+1) / divisions,   brv = (float)(j+1) / divisions;

                verts.push_back({tl, f.normal, tlu, tlv});
                verts.push_back({bl, f.normal, blu, blv});
                verts.push_back({br, f.normal, bru, brv});

                verts.push_back({tl, f.normal, tlu, tlv});
                verts.push_back({br, f.normal, bru, brv});
                verts.push_back({tr, f.normal, tru, trv});
            }
        }
    }
    return verts;
}

std::vector<Vertex> generator::GenerateCylinder(const float radius, const float height,
                                                 const size_t slices, const size_t stacks)
{
    std::vector<Vertex> verts;

    const float slice_size  = 2.0f * (float)M_PI / slices;
    const float half_height = height / 2.0f;
    const float stack_size  = height / stacks;

    // Side faces
    for (int slice = 0; slice < (int)slices; ++slice)
    {
        float a1 = slice * slice_size;
        float a2 = (slice + 1) * slice_size;

        Vec3f n1 = {sinf(a1), 0, cosf(a1)};
        Vec3f n2 = {sinf(a2), 0, cosf(a2)};

        for (int stack = 0; stack < (int)stacks; ++stack)
        {
            float y_bot = stack * stack_size - half_height;
            float y_top = (stack + 1) * stack_size - half_height;

            Vec3f bl = Vec3fPolar(radius, a1, y_bot);
            Vec3f br = Vec3fPolar(radius, a2, y_bot);
            Vec3f tl = Vec3fPolar(radius, a1, y_top);
            Vec3f tr = Vec3fPolar(radius, a2, y_top);

            float u1 = (float)slice / slices,   u2 = (float)(slice+1) / slices;
            float vb = (y_bot + half_height) / height;
            float vt = (y_top + half_height) / height;

            verts.push_back({bl, n1, u1, vb});
            verts.push_back({br, n2, u2, vb});
            verts.push_back({tl, n1, u1, vt});

            verts.push_back({tl, n1, u1, vt});
            verts.push_back({br, n2, u2, vb});
            verts.push_back({tr, n2, u2, vt});
        }
    }

    // Caps
    for (int slice = 0; slice < (int)slices; ++slice)
    {
        float a1 = slice * slice_size;
        float a2 = (slice + 1) * slice_size;

        Vec3f bot_c = {0, -half_height, 0};
        Vec3f top_c = {0,  half_height, 0};
        Vec3f bot_l = Vec3fPolar(radius, a1, -half_height);
        Vec3f bot_r = Vec3fPolar(radius, a2, -half_height);
        Vec3f top_l = Vec3fPolar(radius, a1,  half_height);
        Vec3f top_r = Vec3fPolar(radius, a2,  half_height);

        Vec3f norm_bot = {0, -1, 0};
        Vec3f norm_top = {0,  1, 0};

        float cu = 0.5f, cv = 0.5f;
        float lu1 = 0.5f + 0.5f * sinf(a1), lv1 = 0.5f + 0.5f * cosf(a1);
        float lu2 = 0.5f + 0.5f * sinf(a2), lv2 = 0.5f + 0.5f * cosf(a2);

        verts.push_back({bot_c, norm_bot, cu,  cv });
        verts.push_back({bot_r, norm_bot, lu2, lv2});
        verts.push_back({bot_l, norm_bot, lu1, lv1});

        verts.push_back({top_c, norm_top, cu,  cv });
        verts.push_back({top_l, norm_top, lu1, lv1});
        verts.push_back({top_r, norm_top, lu2, lv2});
    }
    return verts;
}

std::vector<Vertex> generator::GenerateTorus(const float radius, const float tubeRadius,
                                              const size_t slices, const size_t stacks)
{
    std::vector<Vertex> verts;

    for (int stack = 0; stack < (int)stacks; ++stack)
    {
        float theta1 = 2.0f * stack       * (float)M_PI / stacks;
        float theta2 = 2.0f * (stack + 1) * (float)M_PI / stacks;

        for (int slice = 0; slice < (int)slices; ++slice)
        {
            float phi1 = 2.0f * slice       * (float)M_PI / slices;
            float phi2 = 2.0f * (slice + 1) * (float)M_PI / slices;

            auto torusPoint = [&](float theta, float phi) -> Vec3f {
                return {
                    (radius + tubeRadius * cosf(phi)) * cosf(theta),
                    tubeRadius * sinf(phi),
                    (radius + tubeRadius * cosf(phi)) * sinf(theta)
                };
            };
            auto torusNorm = [&](float theta, float phi) -> Vec3f {
                return {cosf(phi) * cosf(theta), sinf(phi), cosf(phi) * sinf(theta)};
            };

            Vec3f tl  = torusPoint(theta1, phi1); Vec3f ntl = torusNorm(theta1, phi1);
            Vec3f tr  = torusPoint(theta2, phi1); Vec3f ntr = torusNorm(theta2, phi1);
            Vec3f bl  = torusPoint(theta1, phi2); Vec3f nbl = torusNorm(theta1, phi2);
            Vec3f br  = torusPoint(theta2, phi2); Vec3f nbr = torusNorm(theta2, phi2);

            float u1 = (float)stack / stacks,   u2 = (float)(stack+1) / stacks;
            float v1 = (float)slice / slices,   v2 = (float)(slice+1) / slices;

            verts.push_back({tl, ntl, u1, v1});
            verts.push_back({bl, nbl, u1, v2});
            verts.push_back({br, nbr, u2, v2});

            verts.push_back({tl, ntl, u1, v1});
            verts.push_back({br, nbr, u2, v2});
            verts.push_back({tr, ntr, u2, v1});
        }
    }
    return verts;
}

std::vector<Vertex> generator::GenerateIcosphere(const float radius, const size_t subdivisions)
{
    if (subdivisions < 1) return {};

    const float t = (1.0f + sqrtf(5.0f)) / 2.0f;

    std::vector<Vec3f> icoVerts = {
        Vec3f{-1, t, 0}.Normalized(),  Vec3f{1, t, 0}.Normalized(),
        Vec3f{-1,-t, 0}.Normalized(),  Vec3f{1,-t, 0}.Normalized(),
        Vec3f{0,-1, t}.Normalized(),   Vec3f{0, 1, t}.Normalized(),
        Vec3f{0,-1,-t}.Normalized(),   Vec3f{0, 1,-t}.Normalized(),
        Vec3f{t, 0,-1}.Normalized(),   Vec3f{t, 0, 1}.Normalized(),
        Vec3f{-t,0,-1}.Normalized(),   Vec3f{-t,0, 1}.Normalized()
    };

    std::vector<std::array<int,3>> icoFaces = {
        {0,11,5},{0,5,1},{0,1,7},{0,7,10},{0,10,11},
        {1,5,9},{5,11,4},{11,10,2},{10,7,6},{7,1,8},
        {3,9,4},{3,4,2},{3,2,6},{3,6,8},{3,8,9},
        {4,9,5},{2,4,11},{6,2,10},{8,6,7},{9,8,1}
    };

    for (size_t i = 0; i < subdivisions - 1; ++i)
    {
        std::vector<Vec3f> newV;
        std::vector<std::array<int,3>> newF;

        for (const auto& face : icoFaces)
        {
            Vec3f a = icoVerts[face[0]];
            Vec3f b = icoVerts[face[1]];
            Vec3f c = icoVerts[face[2]];

            Vec3f ab = ((a + b) * 0.5f).Normalized();
            Vec3f bc = ((b + c) * 0.5f).Normalized();
            Vec3f ca = ((c + a) * 0.5f).Normalized();

            int iA = newV.size(); newV.push_back(a);
            int iB = newV.size(); newV.push_back(b);
            int iC = newV.size(); newV.push_back(c);
            int iAB = newV.size(); newV.push_back(ab);
            int iBC = newV.size(); newV.push_back(bc);
            int iCA = newV.size(); newV.push_back(ca);

            newF.push_back({iA, iAB, iCA});
            newF.push_back({iAB, iB, iBC});
            newF.push_back({iBC, iC, iCA});
            newF.push_back({iAB, iBC, iCA});
        }
        icoVerts = newV;
        icoFaces = newF;
    }

    std::vector<Vertex> verts;
    auto toVertex = [&](Vec3f n) -> Vertex {
        Vec3f pos = n * radius;
        float alpha = atan2f(n.x, n.z);
        float beta  = asinf(n.y);
        float u = (alpha + (float)M_PI) / (2.0f * (float)M_PI);
        float v = (beta  + (float)M_PI_2) / (float)M_PI;
        return {pos, n, u, v};
    };

    if (subdivisions == 1)
    {
        for (const auto& face : icoFaces)
            for (int idx : face)
                verts.push_back(toVertex(icoVerts[idx]));
    }
    else
    {
        for (const auto& n : icoVerts)
            verts.push_back(toVertex(n));
    }

    return verts;
}

// ─── Bezier patch helpers ────────────────────────────────────────────────────

static const float BM[4][4] = {
    {-1.0f,  3.0f, -3.0f, 1.0f},
    { 3.0f, -6.0f,  3.0f, 0.0f},
    {-3.0f,  3.0f,  0.0f, 0.0f},
    { 1.0f,  0.0f,  0.0f, 0.0f}
};

static Vec3f evalBezier(float u, float v, const std::vector<Vec3f>& cp)
{
    float U[4]  = {u*u*u, u*u, u, 1.0f};
    float V[4]  = {v*v*v, v*v, v, 1.0f};

    float UM[4] = {}, MV[4] = {};
    for (int i = 0; i < 4; ++i)
        for (int j = 0; j < 4; ++j) {
            UM[i] += U[j] * BM[j][i];
            MV[i] += BM[i][j] * V[j];
        }

    Vec3f res = {};
    for (int i = 0; i < 4; ++i)
        for (int j = 0; j < 4; ++j) {
            float w = UM[i] * MV[j];
            res.x += cp[i*4+j].x * w;
            res.y += cp[i*4+j].y * w;
            res.z += cp[i*4+j].z * w;
        }
    return res;
}

static Vec3f evalBezierDu(float u, float v, const std::vector<Vec3f>& cp)
{
    float DU[4] = {3*u*u, 2*u, 1, 0};
    float V[4]  = {v*v*v, v*v, v, 1.0f};

    float DUM[4] = {}, MV[4] = {};
    for (int i = 0; i < 4; ++i)
        for (int j = 0; j < 4; ++j) {
            DUM[i] += DU[j] * BM[j][i];
            MV[i]  += BM[i][j] * V[j];
        }

    Vec3f res = {};
    for (int i = 0; i < 4; ++i)
        for (int j = 0; j < 4; ++j) {
            float w = DUM[i] * MV[j];
            res.x += cp[i*4+j].x * w;
            res.y += cp[i*4+j].y * w;
            res.z += cp[i*4+j].z * w;
        }
    return res;
}

static Vec3f evalBezierDv(float u, float v, const std::vector<Vec3f>& cp)
{
    float U[4]   = {u*u*u, u*u, u, 1.0f};
    float DV[4]  = {3*v*v, 2*v, 1, 0};

    float UM[4] = {}, MDV[4] = {};
    for (int i = 0; i < 4; ++i)
        for (int j = 0; j < 4; ++j) {
            UM[i]  += U[j]  * BM[j][i];
            MDV[i] += BM[i][j] * DV[j];
        }

    Vec3f res = {};
    for (int i = 0; i < 4; ++i)
        for (int j = 0; j < 4; ++j) {
            float w = UM[i] * MDV[j];
            res.x += cp[i*4+j].x * w;
            res.y += cp[i*4+j].y * w;
            res.z += cp[i*4+j].z * w;
        }
    return res;
}

static Vec3f cross3(Vec3f a, Vec3f b)
{
    return {a.y*b.z - a.z*b.y, a.z*b.x - a.x*b.z, a.x*b.y - a.y*b.x};
}

std::vector<Vertex> generator::GeneratePatch(const char* filename, int tess)
{
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Could not open patch file: " << filename << std::endl;
        return {};
    }

    int numPatches = 0;
    std::string line;
    if (std::getline(file, line)) numPatches = std::stoi(line);

    std::vector<std::vector<int>> patchIdx(numPatches, std::vector<int>(16));
    for (int i = 0; i < numPatches; ++i) {
        std::getline(file, line);
        std::istringstream iss(line);
        std::string tok;
        for (int j = 0; j < 16; ++j) {
            std::getline(iss, tok, ',');
            patchIdx[i][j] = std::stoi(tok);
        }
    }

    int numCP = 0;
    if (std::getline(file, line)) numCP = std::stoi(line);
    std::vector<Vec3f> cp(numCP);
    for (int i = 0; i < numCP; ++i) {
        std::getline(file, line);
        std::istringstream iss(line);
        std::string tok;
        std::getline(iss, tok, ','); cp[i].x = std::stof(tok);
        std::getline(iss, tok, ','); cp[i].y = std::stof(tok);
        std::getline(iss, tok, ','); cp[i].z = std::stof(tok);
    }

    std::vector<Vertex> verts;
    float step = 1.0f / tess;

    for (const auto& idx : patchIdx) {
        std::vector<Vec3f> pcp(16);
        for (int i = 0; i < 16; ++i) pcp[i] = cp[idx[i]];

        for (int i = 0; i < tess; ++i) {
            for (int j = 0; j < tess; ++j) {
                float u1 = i * step, u2 = (i+1) * step;
                float v1 = j * step, v2 = (j+1) * step;

                auto mkV = [&](float u, float v) -> Vertex {
                    Vec3f pos = evalBezier(u, v, pcp);
                    Vec3f du  = evalBezierDu(u, v, pcp);
                    Vec3f dv  = evalBezierDv(u, v, pcp);
                    Vec3f n   = cross3(dv, du).Normalized();
                    return {pos, n, u, v};
                };

                Vertex p0 = mkV(u1, v1);
                Vertex p1 = mkV(u2, v1);
                Vertex p2 = mkV(u1, v2);
                Vertex p3 = mkV(u2, v2);

                verts.push_back(p0); verts.push_back(p2); verts.push_back(p1);
                verts.push_back(p1); verts.push_back(p2); verts.push_back(p3);
            }
        }
    }
    return verts;
}
