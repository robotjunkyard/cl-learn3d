#pragma once

#include <cmath>

inline bool feq(float a, float b, float epsilon = 1e-6f)
{
    return fabs(a - b) < epsilon;
}

class Vec4 {
public:
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

    Vec4 operator-() const
    {
        return Vec4(-x, -y, -z, -w);
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

class Vec3 {
public:
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
        const float cx = (a.y * b.z) - (a.z * b.y);
        const float cy = (a.z * b.x) - (a.x * b.z);
        const float cz = (a.x * b.y) - (a.y * b.x);
        return Vec3(cx, cy, cz);
    }

    static Vec3 zero()
    {
        return Vec3(0.0f, 0.0f, 0.0f);
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

    Vec3 operator-() const
    {
        return Vec3(-x, -y, -z);
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

class Vec2 {
public:
    Vec2()
        : x(1.0f)
        , y(0.0f)
    {
    }

    ~Vec2() = default;

    Vec2(float x, float y)
        : x(x)
        , y(y)
    {
    }

    Vec2(const Vec3& other)
        : x(other.x)
        , y(other.y)
    {
    }

    static Vec2 zero()
    {
        return Vec2(0.0f, 0.0f);
    }

    static float dot(const Vec2& a, const Vec2& b)
    {
        return (a.x * b.x) + (a.y * b.y);
    }

    Vec2& operator=(const Vec2& other)
    {
        x = other.x;
        y = other.y;
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

    Vec2& operator+=(const Vec2& other)
    {
        x += other.x;
        y += other.y;
        return *this;
    }

    Vec2& operator-=(const Vec2& other)
    {
        x -= other.x;
        y -= other.y;
        return *this;
    }

    Vec2 operator-() const
    {
        return Vec2(-x, -y);
    }

    Vec2& operator*=(const float c)
    {
        x *= c;
        y *= c;
        return *this;
    }

    bool operator==(const Vec2& other) const
    {
        return feq(x, other.x) && feq(y, other.y);
    }

    float mag() const
    {
        return sqrt((x * x) + (y * y));
    }

    Vec2& normalize()
    {
        const float m = mag();
        if (m != 0.0f) { // TODO: use epsilon-compare
            x /= m;
            y /= m;
        }
        return *this;
    }

    union {
        float x;
        float u;
    };

    union {
        float y;
        float v;
    };
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

inline Vec2 operator+(const Vec2& a, const Vec2& b)
{
    return Vec2(a.x + b.x, a.y + b.y);
}

inline Vec2 operator-(const Vec2& a, const Vec2& b)
{
    return Vec2(a.x - b.x, a.y - b.y);
}
