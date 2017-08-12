#pragma once
#include "Vec.hpp"

struct Mat {
    Mat()
        : m11(1.0f)
        , m12(0.0f)
        , m13(0.0f)
        , m14(0.0f)
        , m21(0.0f)
        , m22(1.0f)
        , m23(0.0f)
        , m24(0.0f)
        , m31(0.0f)
        , m32(0.0f)
        , m33(1.0f)
        , m34(0.0f)
        , m41(0.0f)
        , m42(0.0f)
        , m43(0.0f)
        , m44(1.0f)
    {
    }

    Mat(float m11, float m12, float m13, float m14,
        float m21, float m22, float m23, float m24,
        float m31, float m32, float m33, float m34,
        float m41, float m42, float m43, float m44)
        : m11(m11)
        , m12(m12)
        , m13(m13)
        , m14(m14)
        , m21(m21)
        , m22(m22)
        , m23(m23)
        , m24(m24)
        , m31(m31)
        , m32(m32)
        , m33(m33)
        , m34(m34)
        , m41(m41)
        , m42(m42)
        , m43(m43)
        , m44(m44)
    {
    }

    Mat(const Mat& other)
        : m11(other.m11)
        , m12(other.m12)
        , m13(other.m13)
        , m14(other.m14)
        , m21(other.m21)
        , m22(other.m22)
        , m23(other.m23)
        , m24(other.m24)
        , m31(other.m31)
        , m32(other.m32)
        , m33(other.m33)
        , m34(other.m34)
        , m41(other.m41)
        , m42(other.m42)
        , m43(other.m43)
        , m44(other.m44)
    {
    }

    Mat& operator=(const Mat& other)
    {
        m11 = other.m11;
        m12 = other.m12;
        m13 = other.m13;
        m14 = other.m14;
        m21 = other.m21;
        m22 = other.m22;
        m23 = other.m23;
        m24 = other.m24;
        m31 = other.m31;
        m32 = other.m32;
        m33 = other.m33;
        m34 = other.m34;
        m41 = other.m41;
        m42 = other.m42;
        m43 = other.m43;
        m44 = other.m44;

        return *this;
    }

    Mat transposed() const
    {
        return Mat(m11, m21, m31, m41,
            m12, m22, m32, m42,
            m13, m23, m33, m43,
            m14, m24, m34, m44);
    }

    // TODO: Not yet formally tested
    float det(void) const
    {
        const float m11m22m33m44 = m11 * m22 * m33 * m44;
        const float m11m22m34m43 = m11 * m22 * m34 * m43;
        const float m11m23m34m42 = m11 * m23 * m34 * m42;
        const float m11m23m32m44 = m11 * m23 * m32 * m44;
        const float m11m24m32m43 = m11 * m24 * m32 * m43;
        const float m11m24m33m42 = m11 * m24 * m33 * m42;
        const float m12m23m34m41 = m12 * m23 * m34 * m41;
        const float m12m23m31m44 = m12 * m23 * m31 * m44;
        const float m12m24m31m43 = m12 * m24 * m31 * m43;
        const float m12m24m33m41 = m12 * m24 * m33 * m41;
        const float m12m21m33m44 = m12 * m21 * m33 * m44;
        const float m12m21m34m43 = m12 * m21 * m34 * m43;
        const float m13m24m31m42 = m13 * m24 * m31 * m42;
        const float m13m24m32m41 = m13 * m24 * m32 * m41;
        const float m13m21m32m44 = m13 * m21 * m32 * m44;
        const float m13m21m34m42 = m13 * m21 * m34 * m42;
        const float m13m22m34m41 = m13 * m22 * m34 * m41;
        const float m13m22m31m44 = m13 * m22 * m31 * m44;
        const float m14m21m32m43 = m14 * m21 * m32 * m43;
        const float m14m21m33m42 = m14 * m21 * m33 * m42;
        const float m14m22m33m41 = m14 * m22 * m33 * m41;
        const float m14m22m31m43 = m14 * m22 * m31 * m43;
        const float m14m23m31m42 = m14 * m23 * m31 * m42;
        const float m14m23m32m41 = m14 * m23 * m32 * m41;

        return m11m22m33m44 - m11m22m34m43 + m11m23m34m42 - m11m23m32m44
            + m11m24m32m43 - m11m24m33m42 - m12m23m34m41 + m12m23m31m44
            - m12m24m31m43 + m12m24m33m41 - m12m21m33m44 + m12m21m34m43
            + m13m24m31m42 - m13m24m32m41 + m13m21m32m44 - m13m21m34m42
            + m13m22m34m41 - m13m22m31m44 - m14m21m32m43 + m14m21m33m42
            - m14m22m33m41 + m14m22m31m43 - m14m23m31m42 + m14m23m32m41;
    }

    Vec4 operator*(const Vec4& v) const
    {
        return Vec4(m11 * v.x + m21 * v.y + m31 * v.z + m41 * v.w,
            m12 * v.x + m22 * v.y + m32 * v.z + m42 * v.w,
            m13 * v.x + m23 * v.y + m33 * v.z + m43 * v.w,
            m14 * v.x + m24 * v.y + m34 * v.z + m44 * v.w);
    }

    Vec3 operator*(const Vec3& v) const
    {
        return Vec3(m11 * v.x + m21 * v.y + m31 * v.z,
            m12 * v.x + m22 * v.y + m32 * v.z,
            m13 * v.x + m23 * v.y + m33 * v.z);
    }

    static Mat identity()
    {
        return Mat(1.0, 0.0, 0.0, 0.0,
            0.0, 1.0, 0.0, 0.0,
            0.0, 0.0, 1.0, 0.0,
            0.0, 0.0, 0.0, 1.0);
    }

    static Mat zero()
    {
        return Mat(0.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 0.0);
    }

    void print() const;

    float m11, m12, m13, m14,
        m21, m22, m23, m24,
        m31, m32, m33, m34,
        m41, m42, m43, m44;
};

// TODO: THIS NEEDS TO BE TESTED
inline Mat operator*(const Mat& a, const Mat& b)
{
    const float m11 = (a.m11 * b.m11) + (a.m12 * b.m21) + (a.m13 * b.m31) + (a.m14 * b.m41);
    const float m12 = (a.m11 * b.m21) + (a.m12 * b.m22) + (a.m13 * b.m32) + (a.m14 * b.m42);
    const float m13 = (a.m11 * b.m31) + (a.m12 * b.m23) + (a.m13 * b.m33) + (a.m14 * b.m43);
    const float m14 = (a.m11 * b.m41) + (a.m12 * b.m24) + (a.m13 * b.m34) + (a.m14 * b.m44);

    const float m21 = (a.m21 * b.m11) + (a.m22 * b.m21) + (a.m23 * b.m31) + (a.m24 * b.m41);
    const float m22 = (a.m21 * b.m21) + (a.m22 * b.m22) + (a.m23 * b.m32) + (a.m24 * b.m42);
    const float m23 = (a.m21 * b.m31) + (a.m22 * b.m23) + (a.m23 * b.m33) + (a.m24 * b.m43);
    const float m24 = (a.m21 * b.m41) + (a.m22 * b.m24) + (a.m23 * b.m34) + (a.m24 * b.m44);

    const float m31 = (a.m31 * b.m11) + (a.m32 * b.m21) + (a.m33 * b.m31) + (a.m34 * b.m41);
    const float m32 = (a.m31 * b.m21) + (a.m32 * b.m22) + (a.m33 * b.m32) + (a.m34 * b.m42);
    const float m33 = (a.m31 * b.m31) + (a.m32 * b.m23) + (a.m33 * b.m33) + (a.m34 * b.m43);
    const float m34 = (a.m31 * b.m41) + (a.m32 * b.m24) + (a.m33 * b.m34) + (a.m34 * b.m44);

    const float m41 = (a.m41 * b.m11) + (a.m42 * b.m21) + (a.m43 * b.m31) + (a.m44 * b.m41);
    const float m42 = (a.m41 * b.m21) + (a.m42 * b.m22) + (a.m43 * b.m32) + (a.m44 * b.m42);
    const float m43 = (a.m41 * b.m31) + (a.m42 * b.m23) + (a.m43 * b.m33) + (a.m44 * b.m43);
    const float m44 = (a.m41 * b.m41) + (a.m42 * b.m24) + (a.m43 * b.m34) + (a.m44 * b.m44);

    return Mat(m11, m12, m13, m14,
        m21, m22, m23, m24,
        m31, m32, m33, m34,
        m41, m42, m43, m44);
}

inline Mat operator+(const Mat& a, const Mat& b)
{
    return Mat(a.m11 + b.m11, a.m12 + b.m12, a.m13 + b.m13, a.m14 + b.m14,
        a.m21 + b.m21, a.m22 + b.m22, a.m23 + b.m23, a.m24 + b.m24,
        a.m31 + b.m31, a.m32 + b.m32, a.m33 + b.m33, a.m34 + b.m34,
        a.m41 + b.m41, a.m42 + b.m42, a.m43 + b.m43, a.m44 + b.m44);
}

inline Mat operator-(const Mat& a, const Mat& b)
{
    return Mat(a.m11 - b.m11, a.m12 - b.m12, a.m13 - b.m13, a.m14 - b.m14,
        a.m21 - b.m21, a.m22 - b.m22, a.m23 - b.m23, a.m24 - b.m24,
        a.m31 - b.m31, a.m32 - b.m32, a.m33 - b.m33, a.m34 - b.m34,
        a.m41 - b.m41, a.m42 - b.m42, a.m43 - b.m43, a.m44 - b.m44);
}
