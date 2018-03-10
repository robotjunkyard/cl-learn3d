#pragma once
#include <utility>
#include <cstdint>
#include <tuple>
#include "Vec.hpp"

enum class TriangleSide : uint8_t {
    A, B, C
};

class Triangle2 {
public:
    Triangle2 (float ax, float ay, float bx, float by, float cx, float cy)
        : a(ax,ay)
        , b(bx,by)
        , c(cx,cy) {
    }

    Triangle2 (const Vec2& a, const Vec2& b, const Vec2& c)
        : a(a)
        , b(b)
        , c(c) {
    }

    Vec2 a, b, c;

    //! side-length-ish value used for fast "is this side longer than the other?"
    //! or other situations where the actual length itself is inconsequential.
    //! Saves on having to use costly sqrt operation.
    //! TriangleSide::A is the side opposite point a, likewise ::B & b, ::C & c
    float comparativeSideLength(TriangleSide side) const {
        switch (side) {
        case TriangleSide::A:
            return comparativeVertexDistance(b, c);
        case TriangleSide::B:
            return comparativeVertexDistance(a, c);
        case TriangleSide::C:
            return comparativeVertexDistance(a, b);
        }
    }

    Vec3 barycentricCoordinates(const Vec2& point) const;
    Vec2 pointFromBarycentric(const Vec3& barycoord) const;
};


class Triangle3 {
public:
    Triangle3 (float ax, float ay, float az,
               float bx, float by, float bz,
               float cx, float cy, float cz)
        : a(ax,ay,az)
        , b(bx,by,bz)
        , c(cx,cy,cz) {
    }

    Triangle3 (const Vec3& a, const Vec3& b, const Vec3& c)
        : a(a)
        , b(b)
        , c(c) {
    }

    Vec3 a, b, c;

    // TODO?
    // Vec3 barycentricCoordinates(const Vec3& point) const
};

