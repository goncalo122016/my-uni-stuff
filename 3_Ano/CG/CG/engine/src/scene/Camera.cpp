#include "Camera.hpp"

#define _USE_MATH_DEFINES
#include <math.h>

constexpr auto sensitivity = 0.002f;
constexpr auto scroll_sensitivity = 0.2f;

void Camera::ProcessInput(const float x_offset, const float y_offset, const float scroll_offset)
{
    float radius, alpha, beta;
    (position() - looking_at()).ToSpherical(radius, alpha, beta);

    alpha -= x_offset * sensitivity;
    beta -= y_offset * sensitivity;
    radius -= scroll_offset * scroll_sensitivity;

    // Limit vertical rotation to avoid flipping
    if (beta > M_PI_2)
        beta = M_PI_2 - 0.001f;
    else if (beta < -M_PI_2)
        beta = -M_PI_2 + 0.001f;

    // Minimum zoom distance
    if (radius < 0.1f)
        radius = 0.1f;

    const auto after = Vec3fSpherical(radius, alpha, beta);
    position() = after + looking_at();
}

void Camera::FreeLook(float dx, float dy)
{
    constexpr float sens = 0.00008f;
    Vec3f fwd = (looking_at() - position()).Normalized();
    float dist = (looking_at() - position()).Length();

    // Yaw: rotate around world Y axis
    float yaw = -dx * sens;
    Vec3f fwd_yawed = {
        fwd.x * cosf(yaw) + fwd.z * sinf(yaw),
        fwd.y,
        -fwd.x * sinf(yaw) + fwd.z * cosf(yaw)
    };
    fwd_yawed = fwd_yawed.Normalized();

    // Pitch: rotate around right axis (right = (-fwd.z, 0, fwd.x))
    Vec3f right = Vec3f(-fwd_yawed.z, 0.0f, fwd_yawed.x).Normalized();
    float pitch = -dy * sens;
    float cos_p = cosf(pitch), sin_p = sinf(pitch);
    Vec3f cross = {
        right.y * fwd_yawed.z - right.z * fwd_yawed.y,
        right.z * fwd_yawed.x - right.x * fwd_yawed.z,
        right.x * fwd_yawed.y - right.y * fwd_yawed.x
    };
    Vec3f fwd_final = {
        fwd_yawed.x * cos_p + cross.x * sin_p,
        fwd_yawed.y * cos_p + cross.y * sin_p,
        fwd_yawed.z * cos_p + cross.z * sin_p
    };
    if (fwd_final.y >  0.99f) fwd_final.y =  0.99f;
    if (fwd_final.y < -0.99f) fwd_final.y = -0.99f;
    fwd_final = fwd_final.Normalized();

    looking_at() = position() + fwd_final * dist;
}

void Camera::FreeMove(float forward, float strafe, float vertical)
{
    Vec3f fwd = (looking_at() - position()).Normalized();
    Vec3f right = Vec3f(-fwd.z, 0.0f, fwd.x).Normalized();
    Vec3f world_up = {0.0f, 1.0f, 0.0f};

    Vec3f delta = fwd * forward + right * strafe + world_up * vertical;
    position()   = position()   + delta;
    looking_at() = looking_at() + delta;
}

void Camera::Pan(float dx, float dy)
{
    float radius, alpha, beta;
    (position() - looking_at()).ToSpherical(radius, alpha, beta);

    Vec3f forward = (looking_at() - position()).Normalized();
    // right = forward × up
    Vec3f right = {
        forward.y * up().z - forward.z * up().y,
        forward.z * up().x - forward.x * up().z,
        forward.x * up().y - forward.y * up().x
    };
    right = right.Normalized();
    // up_cam = right × forward
    Vec3f up_cam = {
        right.y * forward.z - right.z * forward.y,
        right.z * forward.x - right.x * forward.z,
        right.x * forward.y - right.y * forward.x
    };

    float pan_sensitivity = radius * 0.001f;
    Vec3f delta = right * (-dx * pan_sensitivity) + up_cam * (dy * pan_sensitivity);
    position() = position() + delta;
    looking_at() = looking_at() + delta;
}