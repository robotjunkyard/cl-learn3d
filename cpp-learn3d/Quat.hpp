#pragma once
#include "Mat.hpp"

// thanks to https://www.cprogramming.com/tutorial/3d/quaternions.html for a couple things

class Quat {

public:
    float w, x, y, z;
    Quat(float w, float x, float y, float z)
        : w(w)
        , x(x)
        , y(y)
        , z(z)
    {
        normalize();
    }

    Mat toMatrix() const
    {
        const float m11 = (w * w) + (x * x) - (y * y) - (z * z),
                    m12 = (2 * x * y) - (2 * w * z),
                    m13 = (2 * x * z) + (2 * w * y),
                    m21 = (2 * x * y) + (2 * w * z),
                    m22 = (w * w) - (x * x) + (y * y) - (z * z),
                    m23 = (2 * y * z) - (2 * w * x),
                    m31 = (2 * x * z) - (2 * w * y),
                    m32 = (2 * y * z) + (2 * w * x),
                    m33 = (w * w) - (x * x) - (y * y) + (z * z);
        return Mat(m11, m12, m13, 0.0,
            m21, m22, m23, 0.0,
            m31, m32, m33, 0.0,
            0.0, 0.0, 0.0, 1.0);
    }

    Quat& normalize()
    {
        const float sum = w + x + y + z;
        const float div = sqrt((w * w) + (x * x) + (y * y) + (z * z));
        w /= div;
        x /= div;
        y /= div;
        z /= div;
        return *this;
    }

    ~Quat() {}

    static Quat fromAxisRotation(const Vec3& axis, float angle)
    {
        const float w = cosf(angle / 2.0f),
                    x = axis.x * sinf(angle / 2.0f),
                    y = axis.y * sinf(angle / 2.0f),
                    z = axis.z * sinf(angle / 2.0f);
        return Quat(w, x, y, z);
    }
};

inline Quat
operator*(const Quat& a, const Quat& b)
{
    const float w = (a.w * b.w) - (a.x * b.x) - (a.y * b.y) - (a.z * b.z),
                x = (a.w * b.x) + (a.x * b.w) + (a.y * b.z) - (a.z * b.y),
                y = (a.w * b.y) - (a.x * b.z) + (a.y * b.w) + (a.z * b.x),
                z = (a.w * b.z) + (a.x * b.y) - (a.y * b.x) + (a.z * b.w);
    return Quat(w, x, y, z);
}