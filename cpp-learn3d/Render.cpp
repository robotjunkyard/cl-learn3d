#include "Render.hpp"
#include "Quat.hpp"

void drawFlat3DTriangle(Canvas& canvas,
    const Camera& camera,
    byte color,
    const Vec3& v1, const Vec3& v2, const Vec3& v3,
    const Mat& tmat)
{
    /*static float rot = 0.0f;
    //rot += 0.000025f;
    const Quat rotQuat = Quat::fromAxisRotation(Vec3(0.5, 0.0, 1.0), rot);
    const Mat rotMat = rotQuat.toMatrix();*/
    //const Mat scMat = Mat::scaleMatrix(0.2, 0.2, 0.2);

    // Perf TODO: probably better to do this in one clustered batch for all world geometry
    // to a buffer full of screen-space triangles, and then actually draw from that buffer
    const Vec3 tv1 = tmat * v1; // scMat * rotMat * v1;
    const Vec3 tv2 = tmat * v2; // scMat* rotMat* v2;
    const Vec3 tv3 = tmat * v3; // scMat * rotMat * v3;

    const int w = canvas.width(),
              h = canvas.height();
    const int m = std::min(w, h);

    // TODO: impl. backface culling later
    /*const int sx1 = static_cast<int>((1.0f + tv1.x) * w * 0.5),
              sy1 = static_cast<int>((1.0f - tv1.y) * h * 0.5),
              sx2 = static_cast<int>((1.0f + tv2.x) * w * 0.5),
              sy2 = static_cast<int>((1.0f - tv2.y) * h * 0.5),
              sx3 = static_cast<int>((1.0f + tv3.x) * w * 0.5),
              sy3 = static_cast<int>((1.0f - tv3.y) * h * 0.5);*/
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
    const Mat worldMatrix = Mat::scaleMatrix(0.02) * camera.getViewMatrix() * camera.getProjMatrix();

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