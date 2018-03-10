#pragma once
#include "Vec.hpp"

const float pi = 3.1415926535897932384626433f;

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
        , m44(1.0f) {
    }

    Mat(float M11, float M12, float M13, float M14,
        float M21, float M22, float M23, float M24,
        float M31, float M32, float M33, float M34,
        float M41, float M42, float M43, float M44)
        : m11(M11)
        , m12(M12)
        , m13(M13)
        , m14(M14)
        , m21(M21)
        , m22(M22)
        , m23(M23)
        , m24(M24)
        , m31(M31)
        , m32(M32)
        , m33(M33)
        , m34(M34)
        , m41(M41)
        , m42(M42)
        , m43(M43)
        , m44(M44) {
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
        , m44(other.m44) {
    }

    Mat& operator=(const Mat& other) {
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

    Mat transposed() const {
        return Mat(m11, m21, m31, m41,
                   m12, m22, m32, m42,
                   m13, m23, m33, m43,
                   m14, m24, m34, m44);
    }

    // TODO: Not yet formally tested
    float det(void) const {
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

    Vec4 operator*(const Vec4& v) const {
        return Vec4(m11 * v.x + m21 * v.y + m31 * v.z + m41 * v.w,
                    m12 * v.x + m22 * v.y + m32 * v.z + m42 * v.w,
                    m13 * v.x + m23 * v.y + m33 * v.z + m43 * v.w,
                    m14 * v.x + m24 * v.y + m34 * v.z + m44 * v.w);
    }

    Vec3 operator*(const Vec3& v) const {
        float x = (v.x * m11) + (v.y * m21) + (v.z * m31) + m41;
        float y = (v.x * m12) + (v.y * m22) + (v.z * m32) + m42;
        float z = (v.x * m13) + (v.y * m23) + (v.z * m33) + m43;
        const float w = (v.x * m14) + (v.y * m24) + (v.z * m34) + m44;

        // Saw this fix in other texts that I didn't notice before.
        // Needed because multiplying M4*V3 can sometimes mess up the
        // coordinate-space of a vector, and this nudges it back into
        // whatever space it is supposed to be in.  Something about
        // homogenous vs. Cartesian something-something-or-rather.
        if (feq(w, 0.0f))
            return Vec3(0.0f, 0.0f, 0.0f);

        if (!feq(w, 1.0f)) {
            x /= w;
            y /= w;
            z /= w;
        }

        return Vec3(x, y, z);
    }

    static Mat identity() {
        return Mat(1.0, 0.0, 0.0, 0.0,
                   0.0, 1.0, 0.0, 0.0,
                   0.0, 0.0, 1.0, 0.0,
                   0.0, 0.0, 0.0, 1.0);
    }

    static Mat zero() {
        return Mat(0.0, 0.0, 0.0, 0.0,
                   0.0, 0.0, 0.0, 0.0,
                   0.0, 0.0, 0.0, 0.0,
                   0.0, 0.0, 0.0, 0.0);
    }

    static Mat rotationMatrix(float angle, float ux, float uy, float uz) {
        const float a = angle * (pi / 180.0f),
                    c = cos(a),
                    s = sin(a),
                    omcosa = 1.0f - c;
        const float m11 = (ux * ux * omcosa) + c,
                    m12 = (ux * uy * omcosa) - (uz * s),
                    m13 = (ux * uz * omcosa) + (uy * s),
                    m21 = (uy * ux * omcosa) + (uz * s),
                    m22 = (uy * uy * omcosa) + c,
                    m23 = (uy * uz * omcosa) - (ux * s),
                    m31 = (ux * uz * omcosa) - (uy * s),
                    m32 = (uy * uz * omcosa) + (ux * s),
                    m33 = (uz * uz * omcosa) + c;

        return Mat(m11, m12, m13, 0.0f,
                   m21, m22, m23, 0.0f,
                   m31, m32, m33, 0.0f,
                   0.0f, 0.0f, 0.0f, 1.0f);
    }

    static Mat scaleMatrix(float x, float y, float z) {
        return Mat(x, 0.0f, 0.0f, 0.0f,
                   0.0f, y, 0.0f, 0.0f,
                   0.0f, 0.0f, z, 0.0f,
                   0.0f, 0.0f, 0.0f, 1.0f);
    }

    static Mat scaleMatrix(float scalar) {
        return scaleMatrix(scalar, scalar, scalar);
    }

    void print() const;

    float m11, m12, m13, m14,
          m21, m22, m23, m24,
          m31, m32, m33, m34,
          m41, m42, m43, m44;
};

inline Mat operator*(const Mat& b, const Mat& a) {
    const float m11 = (a.m11 * b.m11) + (a.m12 * b.m21) + (a.m13 * b.m31) + (a.m14 * b.m41);
    const float m12 = (a.m11 * b.m12) + (a.m12 * b.m22) + (a.m13 * b.m32) + (a.m14 * b.m42);
    const float m13 = (a.m11 * b.m13) + (a.m12 * b.m23) + (a.m13 * b.m33) + (a.m14 * b.m43);
    const float m14 = (a.m11 * b.m14) + (a.m12 * b.m24) + (a.m13 * b.m34) + (a.m14 * b.m44);
    const float m21 = (a.m21 * b.m11) + (a.m22 * b.m21) + (a.m23 * b.m31) + (a.m24 * b.m41);
    const float m22 = (a.m21 * b.m12) + (a.m22 * b.m22) + (a.m23 * b.m32) + (a.m24 * b.m42);
    const float m23 = (a.m21 * b.m13) + (a.m22 * b.m23) + (a.m23 * b.m33) + (a.m24 * b.m43);
    const float m24 = (a.m21 * b.m14) + (a.m22 * b.m24) + (a.m23 * b.m34) + (a.m24 * b.m44);
    const float m31 = (a.m31 * b.m11) + (a.m32 * b.m21) + (a.m33 * b.m31) + (a.m34 * b.m41);
    const float m32 = (a.m31 * b.m12) + (a.m32 * b.m22) + (a.m33 * b.m32) + (a.m34 * b.m42);
    const float m33 = (a.m31 * b.m13) + (a.m32 * b.m23) + (a.m33 * b.m33) + (a.m34 * b.m43);
    const float m34 = (a.m31 * b.m14) + (a.m32 * b.m24) + (a.m33 * b.m34) + (a.m34 * b.m44);
    const float m41 = (a.m41 * b.m11) + (a.m42 * b.m21) + (a.m43 * b.m31) + (a.m44 * b.m41);
    const float m42 = (a.m41 * b.m12) + (a.m42 * b.m22) + (a.m43 * b.m32) + (a.m44 * b.m42);
    const float m43 = (a.m41 * b.m13) + (a.m42 * b.m23) + (a.m43 * b.m33) + (a.m44 * b.m43);
    const float m44 = (a.m41 * b.m14) + (a.m42 * b.m24) + (a.m43 * b.m34) + (a.m44 * b.m44);

    return Mat(m11, m12, m13, m14,
               m21, m22, m23, m24,
               m31, m32, m33, m34,
               m41, m42, m43, m44);
}

inline Mat operator+(const Mat& a, const Mat& b) {
    return Mat(a.m11 + b.m11, a.m12 + b.m12, a.m13 + b.m13, a.m14 + b.m14,
               a.m21 + b.m21, a.m22 + b.m22, a.m23 + b.m23, a.m24 + b.m24,
               a.m31 + b.m31, a.m32 + b.m32, a.m33 + b.m33, a.m34 + b.m34,
               a.m41 + b.m41, a.m42 + b.m42, a.m43 + b.m43, a.m44 + b.m44);
}

inline Mat operator-(const Mat& a, const Mat& b) {
    return Mat(a.m11 - b.m11, a.m12 - b.m12, a.m13 - b.m13, a.m14 - b.m14,
               a.m21 - b.m21, a.m22 - b.m22, a.m23 - b.m23, a.m24 - b.m24,
               a.m31 - b.m31, a.m32 - b.m32, a.m33 - b.m33, a.m34 - b.m34,
               a.m41 - b.m41, a.m42 - b.m42, a.m43 - b.m43, a.m44 - b.m44);
}
