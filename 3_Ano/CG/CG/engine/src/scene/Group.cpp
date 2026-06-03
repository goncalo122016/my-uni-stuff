#include "Group.hpp"
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <iostream>
#include <map>
#include <cmath>
#include "../core/Engine.hpp"

// ── Catmull-Rom helpers ───────────────────────────────────────────────────────
// Get current animation time, respecting pause state
float getAnimationTime() {
    if (Engine::g_engine_instance && Engine::g_engine_instance->IsAnimationPaused()) {
        return Engine::g_engine_instance->GetPausedTime();
    }
    return glfwGetTime();
}

void buildRotationMatrix(float *x, float *y, float *z, float *m)
{
    m[0]=x[0]; m[1]=x[1]; m[2]=x[2]; m[3]=0;
    m[4]=y[0]; m[5]=y[1]; m[6]=y[2]; m[7]=0;
    m[8]=z[0]; m[9]=z[1]; m[10]=z[2];m[11]=0;
    m[12]=0;   m[13]=0;   m[14]=0;   m[15]=1;
}
static void crossProduct(float *a, float *b, float *r)
{
    r[0]=a[1]*b[2]-a[2]*b[1]; r[1]=a[2]*b[0]-a[0]*b[2]; r[2]=a[0]*b[1]-a[1]*b[0];
}
static void normalize(float *a)
{
    float l = sqrtf(a[0]*a[0]+a[1]*a[1]+a[2]*a[2]);
    if (l>0){a[0]/=l;a[1]/=l;a[2]/=l;}
}

static void getCatmullRomPoint(float t,
    float *p0, float *p1, float *p2, float *p3,
    float *pos, float *deriv)
{
    float m[4][4]={{-0.5f,1.5f,-1.5f,0.5f},{1.0f,-2.5f,2.0f,-0.5f},
                   {-0.5f,0.0f,0.5f,0.0f},{0.0f,1.0f,0.0f,0.0f}};
    float T[4]={t*t*t,t*t,t,1};
    float Td[4]={3*t*t,2*t,1,0};
    for(int i=0;i<3;i++){
        float P[4]={p0[i],p1[i],p2[i],p3[i]};
        pos[i]=0; deriv[i]=0;
        for(int j=0;j<4;j++){
            float v=0;
            for(int k=0;k<4;k++) v+=m[j][k]*P[k];
            pos[i]+=T[j]*v; deriv[i]+=Td[j]*v;
        }
    }
}

static void getGlobalCatmullRomPoint(float gt, const std::vector<Vec3f>&pts,
                                     float *pos, float *deriv)
{
    int n=(int)pts.size();
    float t=gt*n; int idx=(int)floorf(t); t-=idx;
    int p0=(idx+n-1)%n, p1=idx%n, p2=(idx+1)%n, p3=(idx+2)%n;
    float a[3]={pts[p0].x,pts[p0].y,pts[p0].z};
    float b[3]={pts[p1].x,pts[p1].y,pts[p1].z};
    float c[3]={pts[p2].x,pts[p2].y,pts[p2].z};
    float d[3]={pts[p3].x,pts[p3].y,pts[p3].z};
    getCatmullRomPoint(t,a,b,c,d,pos,deriv);
}

// ── World position tracking ───────────────────────────────────────────────────
static void mat4_mul(const float* a, const float* b, float* out)
{
    for (int j = 0; j < 4; j++)
        for (int i = 0; i < 4; i++) {
            out[j*4+i] = 0;
            for (int k = 0; k < 4; k++) out[j*4+i] += a[k*4+i] * b[j*4+k];
        }
}

// Helper: transform an AABB corner through a column-major 4x4 matrix
static void extendWithTransformedCorners(const float* m, const AABB& local, AABB& out)
{
    const Vec3f& mn = local.min;
    const Vec3f& mx = local.max;
    Vec3f corners[8] = {
        {mn.x,mn.y,mn.z},{mx.x,mn.y,mn.z},{mn.x,mx.y,mn.z},{mx.x,mx.y,mn.z},
        {mn.x,mn.y,mx.z},{mx.x,mn.y,mx.z},{mn.x,mx.y,mx.z},{mx.x,mx.y,mx.z}
    };
    for (const auto& c : corners) {
        out.Extend({
            m[0]*c.x + m[4]*c.y + m[8]*c.z  + m[12],
            m[1]*c.x + m[5]*c.y + m[9]*c.z  + m[13],
            m[2]*c.x + m[6]*c.y + m[10]*c.z + m[14]
        });
    }
}

AABB Group::updateWorldPositions(const float parent[16])
{
    // Compute m_local_matrix = T1 × T2 × ... × TN (this group's transforms only).
    // Also compute m_world_matrix = parent × m_local_matrix.
    // The local matrix is cached so render() can use glMultMatrixf(m_local_matrix)
    // instead of re-evaluating Catmull-Rom / trig in the render pass.
    float local[16] = {1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1};
    float anim_t = getAnimationTime();

    for (const Transform& tr : transforms) {
        float mat[16] = {1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1};
        float tmp[16];
        switch (tr.type) {
        case Transform::Type::TRANSLATE: {
            float tx = tr.x, ty = tr.y, tz = tr.z;
            if (tr.time > 0 && tr.controlPoints.size() >= 4) {
                float gt = fmodf(anim_t / tr.time, 1.0f);
                float pos[3], deriv[3];
                getGlobalCatmullRomPoint(gt, tr.controlPoints, pos, deriv);
                tx = pos[0]; ty = pos[1]; tz = pos[2];
                if (tr.align) {
                    // Include alignment rotation in the local matrix so render()
                    // can use glMultMatrixf without re-evaluating the derivative.
                    float X[3]={deriv[0],deriv[1],deriv[2]}; normalize(X);
                    float Y[3]={0,1,0}, Z[3];
                    crossProduct(X,Y,Z); normalize(Z);
                    crossProduct(Z,X,Y); normalize(Y);
                    float rm[16]; buildRotationMatrix(X,Y,Z,rm);
                    mat[12] = tx; mat[13] = ty; mat[14] = tz;
                    float aligned[16]; mat4_mul(mat, rm, aligned);
                    memcpy(mat, aligned, 64);
                    break;
                }
            }
            mat[12] = tx; mat[13] = ty; mat[14] = tz;
            break;
        }
        case Transform::Type::ROTATE: {
            float angle = tr.angle;
            if (tr.time > 0)
                angle = fmodf((anim_t / tr.time) * 360.0f, 360.0f);
            float a = angle * (float)M_PI / 180.0f;
            float c = cosf(a), s = sinf(a);
            float ax = tr.x, ay = tr.y, az = tr.z;
            float l = sqrtf(ax*ax + ay*ay + az*az);
            if (l > 0) { ax/=l; ay/=l; az/=l; }
            float tc = 1 - c;
            mat[0]=tc*ax*ax+c;    mat[1]=tc*ax*ay+s*az; mat[2]=tc*ax*az-s*ay;
            mat[4]=tc*ax*ay-s*az; mat[5]=tc*ay*ay+c;    mat[6]=tc*ay*az+s*ax;
            mat[8]=tc*ax*az+s*ay; mat[9]=tc*ay*az-s*ax; mat[10]=tc*az*az+c;
            break;
        }
        case Transform::Type::SCALE:
            mat[0]=tr.x; mat[5]=tr.y; mat[10]=tr.z;
            break;
        }
        mat4_mul(local, mat, tmp);
        memcpy(local, tmp, 64);
    }

    memcpy(m_local_matrix, local, 64);

    // world = parent × local
    float world[16];
    mat4_mul(parent, local, world);
    memcpy(m_world_matrix, world, 64);
    m_world_pos = {world[12], world[13], world[14]};

    // Build this group's world AABB: models transformed by world + all children's AABBs
    AABB world_aabb;
    for (const Model& model : models) {
        AABB local_aabb = model.GetAABB();
        if (local_aabb.IsValid())
            extendWithTransformedCorners(world, local_aabb, world_aabb);
    }
    for (Group& child : children) {
        AABB child_aabb = child.updateWorldPositions(world);
        if (child_aabb.IsValid()) {
            world_aabb.Extend(child_aabb.min);
            world_aabb.Extend(child_aabb.max);
        }
    }
    m_world_aabb = world_aabb;
    return world_aabb;
}

// ── Orbit circle rendering ────────────────────────────────────────────────────
void Group::renderOrbitCircle()
{
    if (!Engine::g_engine_instance || !Engine::g_engine_instance->IsRenderOrbits()) return;

    bool has_time_rotate = false;
    Vec3f rot_axis = {0, 1, 0};
    float orbit_radius = 0;

    for (const Transform& tr : transforms) {
        if (tr.type == Transform::Type::ROTATE && tr.time > 0.0f) {
            has_time_rotate = true;
            float l = sqrtf(tr.x*tr.x + tr.y*tr.y + tr.z*tr.z);
            if (l > 0) rot_axis = {tr.x/l, tr.y/l, tr.z/l};
        }
        if (tr.type == Transform::Type::TRANSLATE && tr.time == 0.0f && tr.controlPoints.empty()) {
            orbit_radius = sqrtf(tr.x*tr.x + tr.y*tr.y + tr.z*tr.z);
        }
    }

    if (!has_time_rotate || orbit_radius < 0.001f) return;

    // Two vectors perpendicular to rot_axis
    Vec3f u = (fabsf(rot_axis.y) < 0.99f) ? Vec3f(0,1,0) : Vec3f(1,0,0);
    float d = u.x*rot_axis.x + u.y*rot_axis.y + u.z*rot_axis.z;
    u = Vec3f(u.x - rot_axis.x*d, u.y - rot_axis.y*d, u.z - rot_axis.z*d).Normalized();
    Vec3f v = {
        rot_axis.y*u.z - rot_axis.z*u.y,
        rot_axis.z*u.x - rot_axis.x*u.z,
        rot_axis.x*u.y - rot_axis.y*u.x
    };

    // Lazy-init: upload circle geometry once (static — orbit shape never changes)
    if (m_orbit_circle_vbo == 0) {
        constexpr int SEGS = 128;
        float verts[SEGS * 3];
        for (int i = 0; i < SEGS; i++) {
            float t = (float)i / SEGS * 2.0f * (float)M_PI;
            float c = cosf(t) * orbit_radius, s = sinf(t) * orbit_radius;
            verts[i*3+0] = u.x*c + v.x*s;
            verts[i*3+1] = u.y*c + v.y*s;
            verts[i*3+2] = u.z*c + v.z*s;
        }
        glGenBuffers(1, &m_orbit_circle_vbo);
        glBindBuffer(GL_ARRAY_BUFFER, m_orbit_circle_vbo);
        glBufferData(GL_ARRAY_BUFFER, sizeof(verts), verts, GL_STATIC_DRAW);
        m_orbit_circle_vbo_count = SEGS;
    }

    glDisable(GL_LIGHTING);
    glColor3f(0.25f, 0.35f, 0.65f);
    glDisableClientState(GL_NORMAL_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    glBindBuffer(GL_ARRAY_BUFFER, m_orbit_circle_vbo);
    glVertexPointer(3, GL_FLOAT, 0, nullptr);
    glDrawArrays(GL_LINE_LOOP, 0, m_orbit_circle_vbo_count);
    glEnableClientState(GL_NORMAL_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glEnable(GL_LIGHTING);
}

// ── Transform application ─────────────────────────────────────────────────────
void Group::applyTransforms()
{
    for (const Transform &tr : transforms) {
        switch (tr.type) {
        case Transform::Type::TRANSLATE:
            if (tr.time > 0 && tr.controlPoints.size() >= 4) {
                float gt = fmodf((float)getAnimationTime() / tr.time, 1.0f);
                float pos[3], deriv[3];
                getGlobalCatmullRomPoint(gt, tr.controlPoints, pos, deriv);
                glTranslatef(pos[0], pos[1], pos[2]);
                if (tr.align) {
                    float X[3]={deriv[0],deriv[1],deriv[2]}; normalize(X);
                    float Y[3]={0,1,0}, Z[3];
                    crossProduct(X,Y,Z); normalize(Z);
                    crossProduct(Z,X,Y); normalize(Y);
                    float rm[16]; buildRotationMatrix(X,Y,Z,rm);
                    glMultMatrixf(rm);
                }
            } else {
                glTranslatef(tr.x, tr.y, tr.z);
            }
            break;
        case Transform::Type::ROTATE:
            if (tr.time > 0) {
                float angle = fmodf(((float)getAnimationTime() / tr.time) * 360.0f, 360.0f);
                glRotatef(angle, tr.x, tr.y, tr.z);
            } else {
                glRotatef(tr.angle, tr.x, tr.y, tr.z);
            }
            break;
        case Transform::Type::SCALE:
            glScalef(tr.x, tr.y, tr.z);
            break;
        }
    }
}

void Group::renderCurves()
{
    if (!Engine::g_engine_instance || !Engine::g_engine_instance->IsRenderOrbits()) return;

    bool drew = false;
    for (Transform &t : transforms) {
        if (t.type != Transform::Type::TRANSLATE || t.time <= 0 || t.controlPoints.size() < 4)
            continue;

        if (!drew) {
            glDisable(GL_LIGHTING);
            glColor3f(0.25f, 0.35f, 0.65f);
            glDisableClientState(GL_NORMAL_ARRAY);
            glDisableClientState(GL_TEXTURE_COORD_ARRAY);
            drew = true;
        }

        // Lazy-init: compute Catmull-Rom path vertices once and upload to GPU
        if (t.curve_vbo == 0) {
            constexpr int SEGS = 100;
            float verts[SEGS * 3];
            for (int i = 0; i < SEGS; i++) {
                float pos[3], deriv[3];
                getGlobalCatmullRomPoint((float)i / SEGS, t.controlPoints, pos, deriv);
                verts[i*3+0] = pos[0];
                verts[i*3+1] = pos[1];
                verts[i*3+2] = pos[2];
            }
            glGenBuffers(1, &t.curve_vbo);
            glBindBuffer(GL_ARRAY_BUFFER, t.curve_vbo);
            glBufferData(GL_ARRAY_BUFFER, sizeof(verts), verts, GL_STATIC_DRAW);
            t.curve_vbo_count = SEGS;
        }

        glBindBuffer(GL_ARRAY_BUFFER, t.curve_vbo);
        glVertexPointer(3, GL_FLOAT, 0, nullptr);
        glDrawArrays(GL_LINE_LOOP, 0, t.curve_vbo_count);
    }

    if (drew) {
        glEnableClientState(GL_NORMAL_ARRAY);
        glEnableClientState(GL_TEXTURE_COORD_ARRAY);
        glEnable(GL_LIGHTING);
    }
}

static void renderAABBWireframe(const AABB &aabb)
{
    if (!aabb.IsValid()) return;
    const Vec3f &mn = aabb.min, &mx = aabb.max;
    glDisable(GL_LIGHTING);
    glColor3f(0.0f, 1.0f, 0.0f);
    glBegin(GL_LINES);
    // Bottom face
    glVertex3f(mn.x,mn.y,mn.z); glVertex3f(mx.x,mn.y,mn.z);
    glVertex3f(mx.x,mn.y,mn.z); glVertex3f(mx.x,mn.y,mx.z);
    glVertex3f(mx.x,mn.y,mx.z); glVertex3f(mn.x,mn.y,mx.z);
    glVertex3f(mn.x,mn.y,mx.z); glVertex3f(mn.x,mn.y,mn.z);
    // Top face
    glVertex3f(mn.x,mx.y,mn.z); glVertex3f(mx.x,mx.y,mn.z);
    glVertex3f(mx.x,mx.y,mn.z); glVertex3f(mx.x,mx.y,mx.z);
    glVertex3f(mx.x,mx.y,mx.z); glVertex3f(mn.x,mx.y,mx.z);
    glVertex3f(mn.x,mx.y,mx.z); glVertex3f(mn.x,mx.y,mn.z);
    // Vertical edges
    glVertex3f(mn.x,mn.y,mn.z); glVertex3f(mn.x,mx.y,mn.z);
    glVertex3f(mx.x,mn.y,mn.z); glVertex3f(mx.x,mx.y,mn.z);
    glVertex3f(mx.x,mn.y,mx.z); glVertex3f(mx.x,mx.y,mx.z);
    glVertex3f(mn.x,mn.y,mx.z); glVertex3f(mn.x,mx.y,mx.z);
    glEnd();
    glColor3f(1,1,1);
    glEnable(GL_LIGHTING);
}

void Group::render(uint8_t frustum_mask)
{
    // Hierarchical frustum culling with plane masking:
    // - Test only the planes that weren't already fully satisfied by an ancestor.
    // - If this AABB is fully inside a plane, clear that bit so children skip it.
    // - If this AABB is outside any plane, cull the entire subtree immediately.
    if (Engine::g_engine_instance && Engine::g_engine_instance->FrustumCullingEnabled()) {
        if (m_world_aabb.IsValid()) {
            uint8_t child_mask;
            if (!Engine::g_engine_instance->GetCachedFrustum().HasInsideWithMask(
                    m_world_aabb, frustum_mask, child_mask))
                return;
            frustum_mask = child_mask;
        }
    }

    // Render world-space AABB for debugging (when enabled in settings)
    if (Engine::g_engine_instance && Engine::g_engine_instance->IsRenderAABB())
        renderAABBWireframe(m_world_aabb);

    glPushMatrix();
    renderCurves();
    renderOrbitCircle();
    // Orbit rendering changes GL_ARRAY_BUFFER and glColor — tell Model's caches.
    Model::InvalidateVBOCache();
    glMultMatrixf(m_local_matrix);

    for (Model &model : models)
        model.Render();

    for (Group &child : children)
        child.render(frustum_mask);

    glPopMatrix();
}

void Group::initAllGPU()
{
    for (Model &model : models)
        model.InitGPU();
    for (Group &child : children)
        child.initAllGPU();
}

void Group::clear()
{
    models.clear();
    children.clear();
    transforms.clear();
}

// ── XML loading ───────────────────────────────────────────────────────────────
static Material parseMaterial(tinyxml2::XMLElement *color_elem)
{
    Material mat;
    if (!color_elem) return mat;

    auto readRGB = [&](const char *tag, float *out) {
        if (auto el = color_elem->FirstChildElement(tag)) {
            int r=200, g=200, b=200;
            el->QueryIntAttribute("R", &r);
            el->QueryIntAttribute("G", &g);
            el->QueryIntAttribute("B", &b);
            out[0] = r / 255.0f;
            out[1] = g / 255.0f;
            out[2] = b / 255.0f;
            out[3] = 1.0f;
        }
    };

    readRGB("diffuse",  mat.diffuse);
    readRGB("ambient",  mat.ambient);
    readRGB("specular", mat.specular);
    readRGB("emissive", mat.emissive);

    if (auto sh = color_elem->FirstChildElement("shininess"))
        sh->QueryFloatAttribute("value", &mat.shininess);

    return mat;
}

Group initializeGroupFromXML(tinyxml2::XMLElement *element)
{
    Group group;
    if (!element) return group;

    // Transformations
    for (auto te = element->FirstChildElement("transform"); te;
         te = te->NextSiblingElement("transform"))
    {
        for (auto tr = te->FirstChildElement("translate"); tr;
             tr = tr->NextSiblingElement("translate"))
        {
            if (tr->Attribute("time")) {
                float time = 0; bool align = false;
                tr->QueryFloatAttribute("time", &time);
                tr->QueryBoolAttribute("align", &align);
                std::vector<Vec3f> pts;
                for (auto pe = tr->FirstChildElement("point"); pe;
                     pe = pe->NextSiblingElement("point")) {
                    float x=0,y=0,z=0;
                    pe->QueryFloatAttribute("x",&x);
                    pe->QueryFloatAttribute("y",&y);
                    pe->QueryFloatAttribute("z",&z);
                    pts.push_back({x,y,z});
                }
                group.addTranslateCurve(time, align, pts);
            } else {
                float x=0,y=0,z=0;
                tr->QueryFloatAttribute("x",&x);
                tr->QueryFloatAttribute("y",&y);
                tr->QueryFloatAttribute("z",&z);
                group.addTranslate(x,y,z);
            }
        }

        for (auto ro = te->FirstChildElement("rotate"); ro;
             ro = ro->NextSiblingElement("rotate"))
        {
            if (ro->Attribute("time")) {
                float time=0,x=0,y=0,z=0;
                ro->QueryFloatAttribute("time",&time);
                ro->QueryFloatAttribute("x",&x);
                ro->QueryFloatAttribute("y",&y);
                ro->QueryFloatAttribute("z",&z);
                group.addRotateTime(time,x,y,z);
            } else {
                float angle=0,x=0,y=0,z=0;
                ro->QueryFloatAttribute("angle",&angle);
                ro->QueryFloatAttribute("x",&x);
                ro->QueryFloatAttribute("y",&y);
                ro->QueryFloatAttribute("z",&z);
                group.addRotate(angle,x,y,z);
            }
        }

        for (auto sc = te->FirstChildElement("scale"); sc;
             sc = sc->NextSiblingElement("scale"))
        {
            float x=1,y=1,z=1;
            sc->QueryFloatAttribute("x",&x);
            sc->QueryFloatAttribute("y",&y);
            sc->QueryFloatAttribute("z",&z);
            group.addScale(x,y,z);
        }
    }

    // Models
    if (auto me = element->FirstChildElement("models")) {
        for (auto mo = me->FirstChildElement("model"); mo;
             mo = mo->NextSiblingElement("model"))
        {
            const char *fp = mo->Attribute("file");
            if (!fp) continue;

            auto opt = LoadModelFromFile(fp);
            if (!opt) { std::cerr << "[Group] Failed to load model: " << fp << "\n"; continue; }

            Model &m = opt.value();

            // Skybox flag
            bool is_skybox = false;
            mo->QueryBoolAttribute("skybox", &is_skybox);
            if (is_skybox) m.SetSkybox(true);

            // Texture
            if (auto tex = mo->FirstChildElement("texture")) {
                const char *tf = tex->Attribute("file");
                if (tf) m.SetTexturePath(tf);
            }

            // Color / material
            m.SetMaterial(parseMaterial(mo->FirstChildElement("color")));

            group.addModel(std::move(m));
        }
    }

    // Child groups
    for (auto ce = element->FirstChildElement("group"); ce;
         ce = ce->NextSiblingElement("group"))
        group.addChild(initializeGroupFromXML(ce));

    return group;
}
