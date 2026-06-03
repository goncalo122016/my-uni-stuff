#ifndef VEC_H
#define VEC_H

#include <math.h>

class Vec4f;

class Vec2f
{
public:
    float x, y;

    constexpr Vec2f() : x(0), y(0) {}
    constexpr Vec2f(const float x, const float y) : x(x), y(y) {}
    constexpr Vec2f operator+(const Vec2f &other) const { return {x + other.x, y + other.y}; }
    constexpr Vec2f operator-(const Vec2f &other) const { return {x - other.x, y - other.y}; }
    constexpr Vec2f operator*(const float scalar) const { return {x * scalar, y * scalar}; }
    constexpr float operator[](const int i) const { return i == 0 ? x : y; }
    constexpr float &operator[](const int i) { return i == 0 ? x : y; }
    float Length() const { return sqrtf(x * x + y * y); }
    Vec2f Normalized() const {
        const float len = Length();
        return len > 0 ? Vec2f{x / len, y / len} : Vec2f{0, 0};
    }
};

inline Vec2f Vec2fPolar(const float radius, const float angle)
{
    return {radius * sinf(angle), radius * cosf(angle)};
}

class Vec3f
{
public:
    float x, y, z;

    constexpr Vec3f() : x(0), y(0), z(0) {}
    constexpr Vec3f(const float x, const float y, const float z) : x(x), y(y), z(z) {}
    constexpr Vec3f operator+(const Vec3f &other) const { return {x + other.x, y + other.y, z + other.z}; }
    constexpr Vec3f operator-(const Vec3f &other) const { return {x - other.x, y - other.y, z - other.z}; }
    constexpr Vec3f operator-() const { return {-x, -y, -z}; }  // Unary negation
    constexpr Vec3f operator*(const float scalar) const { return {x * scalar, y * scalar, z * scalar}; }
    constexpr float operator[](const int i) const { return i == 0 ? x : i == 1 ? y : z; }
    constexpr float &operator[](const int i) { return i == 0 ? x : i == 1 ? y : z; }
    constexpr auto with_y(const float new_y) const { return Vec3f{x, new_y, z}; }
    float Length() const { return sqrtf(x * x + y * y + z * z); }
    Vec3f Normalized() const { 
        const float len = Length();
        return len > 0 ? Vec3f{x / len, y / len, z / len} : Vec3f{0, 0, 0};
    }

    // Produto escalar (dot product)
    constexpr float Dot(const Vec3f &other) const {
        return x * other.x + y * other.y + z * other.z;
    }

    // Produto vetorial (cross product)
    constexpr Vec3f Cross(const Vec3f &other) const {
        return {
            y * other.z - z * other.y,
            z * other.x - x * other.z,
            x * other.y - y * other.x
        };
    }

    void ToSpherical(float &radius, float &alpha, float &beta) const
    {
        radius = this->Length();
        alpha = atan2f(this->x, this->z); // arctan(x/z)
        beta = asinf(this->y / radius); // arcsin(y/r)
    }

    Vec4f ToVec4f() const;
};

inline Vec3f Vec3fSpherical(const float radius, const float alpha, const float beta)
{
    return {radius * cosf(beta) * sinf(alpha), radius * sinf(beta), radius * cosf(beta) * cosf(alpha)};
}

inline Vec3f Vec3fPolar(const float radius, const float alpha, const float y = 0)
{
    return {radius * sinf(alpha), y, radius * cosf(alpha)};
}

class Vec4f
{
public:
    float x = 0, y = 0, z = 0, w = 0;
    
    constexpr float operator[](const int i) const
    {
        switch (i)
        {
            case 0:
                return x;
            case 1:
                return y;
            case 2:
                return z;
            default:
                return w;
        }
    }
    constexpr float &operator[](const int i)
    {
        switch (i)
        {
            case 0:
                return x;
            case 1:
                return y;
            case 2:
                return z;
            default:
                return w;
        }
    }
    Vec3f ToVec3f() { return {x, y, z}; }
};

inline Vec4f Vec3f::ToVec4f() const { return {x, y, z, 1}; }

struct Vertex {
    Vec3f position;
    Vec3f normal;
    float u = 0.0f;
    float v = 0.0f;
};

#endif // VEC_H