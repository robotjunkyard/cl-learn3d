#pragma once

#include <cmath>

inline bool feq(float a, float b, float epsilon = 1e-6f)
{
    return fabs(a - b) < epsilon;
}

struct Vec4 {
    Vec4()
        : x(1.0f)
        , y(0.0f)
        , z(0.0f)
        , w(0.0f)
    {
    }

    ~Vec4() = default;

    Vec4(float x, float y, float z)
        : x(x)
        , y(y)
        , z(z)
        , w(0.0f)
    {
    }

    Vec4(float x, float y, float z, float w)
        : x(x)
        , y(y)
        , z(z)
        , w(w)
    {
    }

    Vec4(const Vec4& other)
        : x(other.x)
        , y(other.y)
        , z(other.z)
        , w(other.w)
    {
    }

    static float dot(const Vec4& a, const Vec4& b)
    {
        return (a.x * b.x) + (a.y * b.y) + (a.z * b.z) + (a.w * b.w);
    }

    Vec4& operator=(const Vec4& other)
    {
        x = other.x;
        y = other.y;
        z = other.z;
        w = other.w;
        return *this;
    }

    float& operator[](int idx)
    {
        return ((float*)(&x))[idx];
    }

    Vec4& operator+=(const Vec4& other)
    {
        x += other.x;
        y += other.y;
        z += other.z;
        w += other.w;
        return *this;
    }

    Vec4& operator-=(const Vec4& other)
    {
        x -= other.x;
        y -= other.y;
        z -= other.z;
        w -= other.w;
        return *this;
    }

    Vec4& operator-()
    {
        x = -x;
        y = -y;
        z = -z;
        w = -w;
        return *this;
    }

    Vec4& operator*=(const float c)
    {
        x *= c;
        y *= c;
        z *= c;
        w *= c;
        return *this;
    }

    float mag() const
    {
        return sqrt((x * x) + (y * y) + (z * z) + (w * w));
    }

    Vec4& normalize()
    {
        const float m = mag();
        if (feq(m, 0.0f)) {
            x /= m;
            y /= m;
            z /= m;
            w /= m;
        }
        return *this;
    }

    bool operator==(const Vec4& other) const
    {
        return feq(x, other.x) && feq(y, other.y) && feq(z, other.z) && feq(w, other.w);
    }

    float x, y, z, w;
};

struct Vec3 {
    Vec3()
        : x(1.0f)
        , y(0.0f)
        , z(0.0f)
    {
    }

    ~Vec3() = default;

    Vec3(float x, float y, float z)
        : x(x)
        , y(y)
        , z(z)
    {
    }

    Vec3(const Vec3& other)
        : x(other.x)
        , y(other.y)
        , z(other.z)
    {
    }

    static Vec3 cross(const Vec3& a, const Vec3& b)
    {
        const float cx = (a.x * b.z) - (a.z * b.y);
        const float cy = (a.z * b.x) - (a.x * b.z);
        const float cz = (a.x * b.y) - (a.y * b.x);
        return Vec3(cx, cy, cz);
    }

    static float dot(const Vec3& a, const Vec3& b)
    {
        return (a.x * b.x) + (a.y * b.y) + (a.z * b.z);
    }

    Vec3& operator=(const Vec3& other)
    {
        x = other.x;
        y = other.y;
        z = other.z;
        return *this;
    }

    float& operator[](int idx)
    {
        return ((float*)(&x))[idx];
    }

    const float& operator[](int idx) const
    {
        return ((float*)(&x))[idx];
    }

    Vec3& operator+=(const Vec3& other)
    {
        x += other.x;
        y += other.y;
        z += other.z;
        return *this;
    }

    Vec3& operator-=(const Vec3& other)
    {
        x -= other.x;
        y -= other.y;
        z -= other.z;
        return *this;
    }

    Vec3& operator-()
    {
        x = -x;
        y = -y;
        z = -z;
        return *this;
    }

    Vec3& operator*=(const float c)
    {
        x *= c;
        y *= c;
        z *= c;
        return *this;
    }

    bool operator==(const Vec3& other) const
    {
        return feq(x, other.x) && feq(y, other.y) && feq(z, other.z);
    }

    float mag() const
    {
        return sqrt((x * x) + (y * y) + (z * z));
    }

    Vec3& normalize()
    {
        const float m = mag();
        if (m != 0.0f) { // TODO: use epsilon-compare
            x /= m;
            y /= m;
            z /= m;
        }
        return *this;
    }

    float x, y, z;
};

inline Vec4 operator+(const Vec4& a, const Vec4& b)
{
    return Vec4(a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w);
}

inline Vec4 operator-(const Vec4& a, const Vec4& b)
{
    return Vec4(a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w);
}

inline Vec3 operator+(const Vec3& a, const Vec3& b)
{
    return Vec3(a.x + b.x, a.y + b.y, a.z + b.z);
}

inline Vec3 operator-(const Vec3& a, const Vec3& b)
{
    return Vec3(a.x - b.x, a.y - b.y, a.z - b.z);
}