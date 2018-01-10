#include "Render.hpp"
#include "Quat.hpp"

Vec3 calculateTriNormal(const Vec3& v1, const Vec3& v2, const Vec3& v3)
{
    const Vec3 a = v2 - v1,
               b = v3 - v1;
    return Vec3::cross(a, b);
}

void drawFlat3DTriangle(Canvas& canvas,
    const Camera& camera,
    byte color,
    const Vec3& v1, const Vec3& v2, const Vec3& v3, // world-space vertices
    const Mat& tmat, // matrix to transform vertices from world->clipping
    bool cullBackfaces = true)
{
    // Perf TODO: probably better to do this in one clustered batch for all world geometry
    // to a buffer full of screen-space triangles, and then actually draw from that buffer
    const Vec3 tv1 = tmat * v1; // scMat * rotMat * v1;
    const Vec3 tv2 = tmat * v2; // scMat * rotMat * v2;
    const Vec3 tv3 = tmat * v3; // scMat * rotMat * v3;
    const Vec3 camv = camera.getTarget() - camera.getOrigin();

    if (cullBackfaces && (Vec3::dot(camv, calculateTriNormal(tv1, tv2, tv3)) > 0))
        return;

    const int w = canvas.width(),
              h = canvas.height();
    const int m = std::min(w, h);
    const float cnear = camera.getNear();

    const int sx1 = (w * 0.5f) + (tv1.x * w * 0.5f / std::max(tv1.z, cnear)),
              sy1 = (h * 0.5f) + (tv1.y * h * 0.5f / std::max(tv1.z, cnear)),
              sx2 = (w * 0.5f) + (tv2.x * w * 0.5f / std::max(tv2.z, cnear)),
              sy2 = (h * 0.5f) + (tv2.y * h * 0.5f / std::max(tv2.z, cnear)),
              sx3 = (w * 0.5f) + (tv3.x * w * 0.5f / std::max(tv3.z, cnear)),
              sy3 = (h * 0.5f) + (tv3.y * h * 0.5f / std::max(tv3.z, cnear));

    if (!((sx1 < 0) && (sx1 > w) && (sx2 < 0) && (sx2 > w) && (sx3 < 0) && (sx3 > w) && (sy1 < 0) && (sy1 > h) && (sy2 < 0) && (sy2 > h) && (sy3 < 0) && (sy3 > h)))
        canvas.drawTriangle(sx1, sy1, sx2, sy2, sx3, sy3, color);
}

void drawMesh(Canvas& canvas, const Camera& camera, const Mesh& mesh)
{
    // const Mat worldMatrix = Mat::scaleMatrix(0.02) * camera.getViewMatrix() * camera.getProjMatrix();
    const Mat worldMatrix = camera.getProjMatrix() * camera.getViewMatrix() * Mat::scaleMatrix(0.02);

    // **HUGE** TODO: re-impl. SORT-MESH-FACE-DRAW-ORDER, which in CL-LEARN3D
    // done w/ a customized version of quicksort that took a predicate
    // function.  Wonder if that can be just as elegantly done in C++14's
    // lambda function facilities, which I haven't yet messed w/ very much...
    for (int faceNum = 0; faceNum < mesh.getFaces().size(); faceNum++) {
        // another less important but still necessary TODO: materials!
        byte color = 2 + (faceNum % 30);
        Vec3 v0, v1, v2;
        mesh.getMeshFaceVertices(faceNum, v0, v1, v2);

        drawFlat3DTriangle(canvas, camera, color, v0, v1, v2, worldMatrix);
    }
}