#include "Triangle.hpp"

Vec3 Triangle2::barycentricCoordinates(const Vec2& point) const {
    const Vec2 v0 = b - a,
               v1 = c - a,
               v2 = point - a;
    const auto g = ((v0.x * v1.y) - (v1.x * v0.y));
    if (feq(g, 0.0f))         // triangle is probably degenerate
        return Vec3::zero();  // ... hmm ...
    const auto gr = 1.0f / g,
               bb = ((v2.x * v1.y) - (v1.x * v2.y)) * gr,
               bc = ((v0.x * v2.y) - (v2.x * v0.y)) * gr,
               ba = 1.0f - bb - bc;
    return Vec3(ba, bb, bc);
}

Vec2 Triangle2::pointFromBarycentric(const Vec3& barycoord) const {
    const float u = barycoord.x,
                v = barycoord.y,
                w = barycoord.z;
    const float x = (u * a.x) + (v * b.x) + (w * c.x),
                y = (u * a.y) + (v * b.y) + (w * c.y);
    return Vec2(x, y);
}
