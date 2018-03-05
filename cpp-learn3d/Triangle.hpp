#pragma once
#include <utility>
#include <cstdint>
#include <tuple>
#include "Vec.hpp"

enum class TriangleSide : uint8_t
{
    A, B, C
};

class Triangle2
{
public:
    Triangle2 (float ax, float ay, float bx, float by, float cx, float cy)
        : a(ax,ay)
        , b(bx,by)
        , c(cx,cy)
    { }

    Triangle2 (const Vec2& a, const Vec2& b, const Vec2& c)
        : a(a)
        , b(b)
        , c(c)
    { }

    Vec2 a, b, c;

    //! side-length-ish value used for fast "is this side longer than the other?"
    //! or other situations where the actual length itself is inconsequential.
    //! Saves on having to use costly sqrt operation.
    //! TriangleSide::A is the side opposite point a, likewise ::B & b, ::C & c
    float comparativeSideLength(TriangleSide side) const
    {
        switch (side)
        {
        case TriangleSide::A:
            return comparativeVertexDistance(b, c);
        case TriangleSide::B:
            return comparativeVertexDistance(a, c);
        case TriangleSide::C:
            return comparativeVertexDistance(a, b);
        }
    }

    Vec3 barycentricCoordinates(const Vec2& point) const;
};
