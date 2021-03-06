#pragma once

#include <limits>
#include <cmath>

inline bool feq(float a, float b, float epsilon = std::numeric_limits<float>::epsilon()) {
    return fabs(a - b) < epsilon;
}

class Vec4 {
public:
    Vec4()
        : x(1.0f)
        , y(0.0f)
        , z(0.0f)
        , w(0.0f) {
    }

    ~Vec4() = default;

    Vec4(float X, float Y, float Z)
        : x(X)
        , y(Y)
        , z(Z)
        , w(0.0f) {
    }

    Vec4(float X, float Y, float Z, float W)
        : x(X)
        , y(Y)
        , z(Z)
        , w(W) {
    }

    Vec4(const Vec4& other)
        : x(other.x)
        , y(other.y)
        , z(other.z)
        , w(other.w) {
    }

    static float dot(const Vec4& a, const Vec4& b) {
        return (a.x * b.x) + (a.y * b.y) + (a.z * b.z) + (a.w * b.w);
    }

    Vec4& operator=(const Vec4& other) {
        x = other.x;
        y = other.y;
        z = other.z;
        w = other.w;
        return *this;
    }

    float& operator[](int idx) {
        return ((float*)(&x))[idx];
    }

    Vec4& operator+=(const Vec4& other) {
        x += other.x;
        y += other.y;
        z += other.z;
        w += other.w;
        return *this;
    }

    Vec4& operator-=(const Vec4& other) {
        x -= other.x;
        y -= other.y;
        z -= other.z;
        w -= other.w;
        return *this;
    }

    Vec4 operator-() const {
        return Vec4(-x, -y, -z, -w);
    }

    Vec4 operator-(const Vec4& other) const {
        return Vec4(x - other.x, y - other.y, z - other.z, w - other.w);
    }

    Vec4& operator*=(const float c) {
        x *= c;
        y *= c;
        z *= c;
        w *= c;
        return *this;
    }

    float mag() const {
        return sqrt((x * x) + (y * y) + (z * z) + (w * w));
    }

    Vec4& normalize() {
        const float m = mag();
        if (!feq(m, 0.0f)) {
            x /= m;
            y /= m;
            z /= m;
            w /= m;
        }
        return *this;
    }

    bool operator==(const Vec4& other) const {
        return feq(x, other.x) && feq(y, other.y) && feq(z, other.z) && feq(w, other.w);
    }

    float x, y, z, w;
};

class Vec3 {
public:
    Vec3()
        : x(1.0f)
        , y(0.0f)
        , z(0.0f) {
    }

    ~Vec3() = default;

    Vec3(float X, float Y, float Z)
        : x(X)
        , y(Y)
        , z(Z) {
    }

    Vec3(const Vec3& other)
        : x(other.x)
        , y(other.y)
        , z(other.z) {
    }

    static Vec3 cross(const Vec3& a, const Vec3& b) {
        const float cx = (a.y * b.z) - (a.z * b.y);
        const float cy = (a.z * b.x) - (a.x * b.z);
        const float cz = (a.x * b.y) - (a.y * b.x);
        return Vec3(cx, cy, cz);
    }

    static Vec3 zero() {
        return Vec3(0.0f, 0.0f, 0.0f);
    }

    static float dot(const Vec3& a, const Vec3& b) {
        return (a.x * b.x) + (a.y * b.y) + (a.z * b.z);
    }

    Vec3& operator=(const Vec3& other) {
        x = other.x;
        y = other.y;
        z = other.z;
        return *this;
    }

    float& operator[](int idx) {
        return ((float*)(&x))[idx];
    }

    const float& operator[](int idx) const {
        return ((float*)(&x))[idx];
    }

    Vec3& operator+=(const Vec3& other) {
        x += other.x;
        y += other.y;
        z += other.z;
        return *this;
    }

    Vec3& operator-=(const Vec3& other) {
        x -= other.x;
        y -= other.y;
        z -= other.z;
        return *this;
    }

    Vec3 operator-() const {
        return Vec3(-x, -y, -z);
    }

    Vec3 operator-(const Vec3& other) const {
        return Vec3(x - other.x, y - other.y, z - other.z);
    }

    Vec3& operator*=(const float c) {
        x *= c;
        y *= c;
        z *= c;
        return *this;
    }

    bool operator==(const Vec3& other) const {
        return feq(x, other.x) && feq(y, other.y) && feq(z, other.z);
    }

    float mag() const {
        return sqrt((x * x) + (y * y) + (z * z));
    }

    Vec3& normalize() {
        const float m = mag();
        if (!feq(m, 0.0f)) {
            x /= m;
            y /= m;
            z /= m;
        }

        return *this;
    }

    bool allGTE(float c) const {
        return (x >= c) && (y >= c) && (z >= c);
    }

    bool allGT(float c) const {
        return (x > c) && (y > c) && (z > c);
    }

    bool allLTE(float c) const {
        return (x <= c) && (y <= c) && (z <= c);
    }

    bool allLT(float c) const {
        return (x < c) && (y < c) && (z < c);
    }

    float x, y, z;
};

class Vec2 {
public:
    Vec2()
        : x(1.0f)
        , y(0.0f) {
    }

    ~Vec2() = default;

    Vec2(float X, float Y)
        : x(X)
        , y(Y) {
    }

    Vec2(const Vec3& other)
        : x(other.x)
        , y(other.y) {
    }

    static float dot(const Vec2& a, const Vec2& b) {
        return (a.x * b.x) + (a.y * b.y);
    }

    Vec2& operator=(const Vec2& other) {
        x = other.x;
        y = other.y;
        return *this;
    }

    float& operator[](int idx) {
        return ((float*)(&x))[idx];
    }

    const float& operator[](int idx) const {
        return ((float*)(&x))[idx];
    }

    Vec2& operator+=(const Vec2& other) {
        x += other.x;
        y += other.y;
        return *this;
    }

    Vec2& operator-=(const Vec2& other) {
        x -= other.x;
        y -= other.y;
        return *this;
    }

    Vec2 operator-() const {
        return Vec2(-x, -y);
    }

    Vec2 operator-(const Vec2& other) const {
        return Vec2(x - other.x, y - other.y);
    }

    Vec2& operator*=(const float c) {
        x *= c;
        y *= c;
        return *this;
    }

    Vec2 operator* (float c) const {
        return Vec2(x*c, y*c);
    }

    bool operator==(const Vec2& other) const {
        return feq(x, other.x) && feq(y, other.y);
    }

    float mag() const {
        return sqrt((x * x) + (y * y));
    }

    Vec2& normalize() {
        const float m = mag();
        if (!feq(m, 0.0f)) {
            x /= m;
            y /= m;
        }
        return *this;
    }

    bool allGTE(float c) const {
        return (x >= c) && (y >= c);
    }

    bool allGT(float c) const {
        return (x > c) && (y > c);
    }

    bool allLTE(float c) const {
        return (x <= c) && (y <= c);
    }

    bool allLT(float c) const {
        return (x < c) && (y < c);
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

// ====================================================================================
// comparative vertex distance means it is not the LITERAL distance between
// two vertices, but a value that's good enough for comparing two distances.
// Basically, this omits slower sqrt calculations which are unnecessary
// if you don't care about the ACTUAL distances--ONLY care about comparing them
inline float comparativeVertexDistance(const Vec3& a, const Vec3& b) {
    const float axbx = a.x - b.x,
                ayby = a.y - b.y,
                azbz = a.z - b.z,
                result = (axbx * axbx) + (ayby * ayby) + (azbz * azbz);
    return fabs(result);
}

inline float comparativeVertexDistance(const Vec2& a, const Vec2& b) {
    const float axbx = a.x - b.x,
                ayby = a.y - b.y,
                result = (axbx * axbx) + (ayby * ayby);
    return fabs(result);
}

