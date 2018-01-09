#include "Render.hpp"

void drawFlat3DTriangle(Canvas& canvas, byte color,
    const Vec3& v1, const Vec3& v2, const Vec3& v3,
    const Mat& tmat)
{
    // Perf TODO: probably better to do this in one clustered batch for all world geometry
    // to a buffer full of screen-space triangles, and then actually draw from that buffer
    const Vec3 tv1 = tmat * v1;
    const Vec3 tv2 = tmat * v2;
    const Vec3 tv3 = tmat * v3;

    const int w = canvas.width(),
              h = canvas.height();
    // TODO: impl. backface culling later
    const int sx1 = static_cast<int>((1.0f + tv1.x) * w * 0.5f),
              sy1 = static_cast<int>((1.0f - tv1.y) * h * 0.5f),
              sx2 = static_cast<int>((1.0f + tv2.x) * w * 0.5f),
              sy2 = static_cast<int>((1.0f - tv2.y) * h * 0.5f),
              sx3 = static_cast<int>((1.0f + tv3.x) * w * 0.5f),
              sy3 = static_cast<int>((1.0f - tv3.y) * h * 0.5f);

    canvas.drawTriangle(sx1, sy1, sx2, sy2, sx3, sy3, color);
}

void drawMesh(Canvas& canvas, const Camera& camera, const Mesh& mesh)
{
    static float rot = 0.0f;
    rot += 2.0f;
    const Mat worldMatrix = camera.getProjMatrix() * camera.getViewMatrix() * Mat::rotationMatrix(rot, 0.0f, 1.0f, 0.0f) * Mat::scaleMatrix(0.2, 0.2, 0.2);

    // **HUGE** TODO: re-impl. SORT-MESH-FACE-DRAW-ORDER, which in CL-LEARN3D
    // done w/ a customized version of quicksort that took a predicate
    // function.  Wonder if that can be just as elegantly done in C++14's
    // lambda function facilities, which I haven't yet messed w/ very much...
    for (int faceNum = 0; faceNum < mesh.getFaces().size(); faceNum++) {
        // another less important but still necessary TODO: materials!
        Vec3 v0, v1, v2;
        byte color = 2 + (faceNum % 30);
        mesh.getMeshFaceVertices(faceNum, v0, v1, v2);
        drawFlat3DTriangle(canvas, color, v0, v1, v2, worldMatrix);
    }
}