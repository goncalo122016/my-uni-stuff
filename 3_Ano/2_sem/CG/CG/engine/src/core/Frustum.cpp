#include "Frustum.hpp"

// Based on https://learnopengl.com/Guest-Articles/2021/Scene/Frustum-Culling

Frustum CreateFrustumFromCamera(
    const Vec3f &cameraPosition,
    const Vec3f &cameraLookingAt,
    const Vec3f &cameraUp,
    float fovDegrees,
    float aspectRatio,
    float near,
    float far)
{
    Frustum frustum;
    const float halfVSide = far * tanf(fovDegrees * (float)M_PI / 180.0f * .5f);
    const float halfHSide = halfVSide * aspectRatio;
    const Vec3f front = (cameraLookingAt - cameraPosition).Normalized();
    const Vec3f frontMultFar = front * far;

    const Vec3f right = front.Cross(cameraUp).Normalized();
    const Vec3f up = right.Cross(front).Normalized();

    frustum.nearFace   = {cameraPosition + (front * near), front};
    frustum.farFace    = {cameraPosition + frontMultFar, -front};
    frustum.leftFace   = {cameraPosition, (frontMultFar - (right * halfHSide)).Cross(up)};
    frustum.rightFace  = {cameraPosition, up.Cross(frontMultFar + (right * halfHSide))};
    frustum.topFace    = {cameraPosition, right.Cross(frontMultFar - (up * halfVSide))};
    frustum.bottomFace = {cameraPosition, (frontMultFar + (up * halfVSide)).Cross(right)};

    return frustum;
}
