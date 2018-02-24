#include "Render.hpp"
#include "PredicateQuicksort.hpp"
#include "Quat.hpp"

Vec3 calculateTriNormal(const Vec3& v1, const Vec3& v2, const Vec3& v3)
{
    const Vec3 a = v2 - v1,
               b = v3 - v1;
    return Vec3::cross(a, b);
}

void Render::drawFlat3DTriangle(Canvas& canvas,
    const Camera& camera,
    byte color,
    const Vec3& v1, const Vec3& v2, const Vec3& v3, // world-space vertices
    const Mat& tmat, // matrix to transform vertices from world->clipping
    bool cullBackfaces = true)
{
    // Perf TODO: probably better to do this in one clustered batch for all world geometry
    // to a buffer full of screen-space triangles, and then actually draw from that buffer
    const Vec3 tv1 = tmat * v1,
               tv2 = tmat * v2,
               tv3 = tmat * v3,
               camv = tmat * (camera.getTarget() - camera.getOrigin());

    if (cullBackfaces && (Vec3::dot(camv, calculateTriNormal(tv1, tv2, tv3)) >= 0.0f))
        return;

    const int w = canvas.width(),
              h = canvas.height();
    const float cnear = camera.getNear();

    const std::function<float(float)> rfunc = &ceilf;
    const int sx1 = static_cast<int>(rfunc((w * 0.5f) + (tv1.x * w * 0.5f / std::max(tv1.z, cnear)))),
              sy1 = static_cast<int>(rfunc((h * 0.5f) + (tv1.y * h * 0.5f / std::max(tv1.z, cnear)))),
              sx2 = static_cast<int>(rfunc((w * 0.5f) + (tv2.x * w * 0.5f / std::max(tv2.z, cnear)))),
              sy2 = static_cast<int>(rfunc((h * 0.5f) + (tv2.y * h * 0.5f / std::max(tv2.z, cnear)))),
              sx3 = static_cast<int>(rfunc((w * 0.5f) + (tv3.x * w * 0.5f / std::max(tv3.z, cnear)))),
              sy3 = static_cast<int>(rfunc((h * 0.5f) + (tv3.y * h * 0.5f / std::max(tv3.z, cnear))));

    if (!((sx1 < 0) && (sx1 > w) && (sx2 < 0) && (sx2 > w) && (sx3 < 0) && (sx3 > w) && (sy1 < 0) && (sy1 > h) && (sy2 < 0) && (sy2 > h) && (sy3 < 0) && (sy3 > h)))
        canvas.drawTriangle(sx1, sy1, sx2, sy2, sx3, sy3, color);
}

void Render::drawMesh(Canvas& canvas, const Camera& camera, const Mesh& mesh)
{
    const Mat worldMatrix = camera.getProjMatrix() * camera.getViewMatrix();
    mesh.sortMeshTriangleDrawOrderFromCamera(worldMatrix, camera);

    // **HUGE** TODO: re-impl. SORT-MESH-FACE-DRAW-ORDER, which in CL-LEARN3D
    // done w/ a customized version of quicksort that took a predicate
    // function.  Wonder if that can be just as elegantly done in C++14's
    // lambda function facilities, which I haven't yet messed w/ very much...
    for (const auto faceNum : (const_cast<Mesh&>(mesh)).getFaceSortBuffer()) {
        // another less important but still necessary TODO: materials!
        const byte color = 2 + (faceNum % 30);
        Vec3 v1, v2, v3;
        mesh.getMeshFaceVertices(faceNum, v1, v2, v3);

        drawFlat3DTriangle(canvas, camera, color, v1, v2, v3, worldMatrix);
    }
}
