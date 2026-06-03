#ifndef GROUP_H
#define GROUP_H

#include <tinyxml2.h>
#include <vector>
#include "../model/Model.hpp"
#include "../../../common/Vec.hpp"

struct Transform {
    enum class Type { TRANSLATE, ROTATE, SCALE };

    Type type;
    float x, y, z;
    float angle = 0;
    float time  = 0;
    bool  align = false;
    std::vector<Vec3f> controlPoints;

    // Pre-computed VBO for Catmull-Rom path rendering (lazy-init in renderCurves).
    // Shape is static (control points fixed), so we upload once and reuse every frame.
    GLuint curve_vbo       = 0;
    int    curve_vbo_count = 0;

    Transform(Type t, float x=0, float y=0, float z=0,
              float angle=0, float time=0, bool align=false)
        : type(t), x(x), y(y), z(z), angle(angle), time(time), align(align) {}
};

class Group {
    std::vector<Group>     children;
    std::vector<Model>     models;
    std::vector<Transform> transforms;

public:
    Group() = default;

    void render(uint8_t frustum_mask = 0x3Fu);
    void initAllGPU(); // eagerly upload all models in this subtree before first frame
    void renderCurves();
    void renderOrbitCircle();

    void addChild(Group child)    { children.push_back(std::move(child)); }
    void addModel(Model model)    { models.push_back(std::move(model)); }

    void addTransform(Transform t)          { transforms.push_back(std::move(t)); }
    void addTranslate(float x, float y, float z) {
        transforms.push_back(Transform(Transform::Type::TRANSLATE, x, y, z));
    }
    void addTranslateCurve(float time, bool align, const std::vector<Vec3f> &pts) {
        Transform t(Transform::Type::TRANSLATE, 0, 0, 0, 0, time, align);
        t.controlPoints = pts;
        transforms.push_back(std::move(t));
    }
    void addRotate(float angle, float x, float y, float z) {
        transforms.push_back(Transform(Transform::Type::ROTATE, x, y, z, angle));
    }
    void addRotateTime(float time, float x, float y, float z) {
        transforms.push_back(Transform(Transform::Type::ROTATE, x, y, z, 0, time));
    }
    void addScale(float x, float y, float z) {
        transforms.push_back(Transform(Transform::Type::SCALE, x, y, z));
    }

    const std::vector<Transform> &getTransforms() const { return transforms; }
    const std::vector<Model>     &getModels()     const { return models; }
    const std::vector<Group>     &getChildren()   const { return children; }

    Vec3f worldPos()    const { return m_world_pos; }
    const AABB& worldAABB() const { return m_world_aabb; }

    // Returns the world-space AABB of this group and all descendants
    AABB updateWorldPositions(const float parent[16]);

    void clear();

private:
    Vec3f m_world_pos    = {};
    AABB  m_world_aabb;           // world-space AABB for this group + all descendants
    float m_world_matrix[16] = {1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1};
    // Local transform matrix (this group's transforms only, not parent-accumulated).
    // Computed once per frame in updateWorldPositions(); used in render() via
    // glMultMatrixf — avoids re-evaluating Catmull-Rom / trig in the render pass.
    float m_local_matrix[16] = {1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1};
    // Pre-computed VBO for orbit circle (lazy-init in renderOrbitCircle).
    GLuint m_orbit_circle_vbo       = 0;
    int    m_orbit_circle_vbo_count = 0;
    void applyTransforms();
};

Group initializeGroupFromXML(tinyxml2::XMLElement *element);

#endif // GROUP_H
