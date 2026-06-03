#ifndef CAMERA_H
#define CAMERA_H

#include "Vec.hpp"

class Camera
{
private:
    Vec3f m_position = Vec3f(5.0f, 5.0f, 5.0f);
    Vec3f m_looking_at = Vec3f(0.0f, 0.0f, 0.0f);
    Vec3f m_up = Vec3f(0.0f, 1.0f, 0.0f);
    float m_fov = 60.0f;
    float m_near = 1.0f;
    float m_far = 1000.0f;

public:
    Camera() = default;

    Vec3f& position() { return m_position; }
    const Vec3f& position() const { return m_position; }
    
    Vec3f& looking_at() { return m_looking_at; }
    const Vec3f& looking_at() const { return m_looking_at; }
    
    Vec3f& up() { return m_up; }
    const Vec3f& up() const { return m_up; }
    
    float& fov() { return m_fov; }
    float fov() const { return m_fov; }
    
    float& near() { return m_near; }
    float near() const { return m_near; }
    
    float& far() { return m_far; }
    float far() const { return m_far; }

    void ProcessInput(float x_offset, float y_offset, float scroll_offset);
    void Pan(float dx, float dy);
    void FreeLook(float dx, float dy);
    void FreeMove(float forward, float strafe, float vertical);
};

#endif // CAMERA_H