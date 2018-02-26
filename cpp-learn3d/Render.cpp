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

    const std::function<float(float)> rfunc = ceilf;
    const int sx1 = static_cast<int>(rfunc((w * 0.5f) + (tv1.x * w * 0.5f / std::max(tv1.z, cnear)))),
              sy1 = static_cast<int>(rfunc((h * 0.5f) + (tv1.y * h * 0.5f / std::max(tv1.z, cnear)))),
              sx2 = static_cast<int>(rfunc((w * 0.5f) + (tv2.x * w * 0.5f / std::max(tv2.z, cnear)))),
              sy2 = static_cast<int>(rfunc((h * 0.5f) + (tv2.y * h * 0.5f / std::max(tv2.z, cnear)))),
              sx3 = static_cast<int>(rfunc((w * 0.5f) + (tv3.x * w * 0.5f / std::max(tv3.z, cnear)))),
              sy3 = static_cast<int>(rfunc((h * 0.5f) + (tv3.y * h * 0.5f / std::max(tv3.z, cnear))));

    if (!((sx1 < 0) && (sx1 > w) && (sx2 < 0) && (sx2 > w) && (sx3 < 0) && (sx3 > w) && (sy1 < 0) && (sy1 > h) && (sy2 < 0) && (sy2 > h) && (sy3 < 0) && (sy3 > h)))
        canvas.drawFlatTriangle(sx1, sy1, sx2, sy2, sx3, sy3, color);
}

void Render::drawMeshFlat(Canvas& canvas, const Camera& camera, const Mesh& mesh)
{
    const Mat worldMatrix = camera.getProjMatrix() * camera.getViewMatrix();
    mesh.sortMeshTriangleDrawOrderFromCamera(worldMatrix, camera);

    for (const auto faceNum : mesh.getFaceSortBuffer()) {
        // another less important but still necessary TODO: materials!
        const byte color = 2 + (faceNum % 30);
        Vec3 v1, v2, v3;
        mesh.getMeshFaceVertices(faceNum, v1, v2, v3);

        drawFlat3DTriangle(canvas, camera, color, v1, v2, v3, worldMatrix);
    }
}

/* --------------------------------------------------------------------------- */

void Render::drawMeshTriangle(Canvas& canvas, const Mesh& mesh, int facenum,
                              int x1, int y1, int x2, int y2, int x3, int y3)
{
    int topx = x1, topy = y1,
        midx = x2, midy = y2,
        btmx = x3, btmy = y3;

    if (topy > midy)
        swap2pair(topx, midx, topy, midy);
    if (topy > btmy)
        swap2pair(topx, btmx, topy, btmy);
    if (midy > btmy)
        swap2pair(midx, btmx, midy, btmy);

    // TRIDBGMSG("--- tri (%d, %d)-(%d, %d)-(%d, %d) ---\n", topx, topy, midx, midy, btmx, btmy);

    const int x_mid_sub_top = midx - topx,
              x_btm_sub_top = btmx - topx,
              x_btm_sub_mid = btmx - midx,
              y_mid_sub_top = midy - topy,
              y_btm_sub_top = btmy - topy,
              y_btm_sub_mid = btmy - midy;

    if ((0 == y_btm_sub_top) || (btmy < 0) || (topy >= canvas.height())) {
        // TRIDBGMSG("Tri FAIL(*)\n");
        return;
    }

    const float dlong = static_cast<float>(x_btm_sub_top) / static_cast<float>(y_btm_sub_top);

    float sx0 = static_cast<float>(topx),
          sx1 = sx0;

    if (0 == y_mid_sub_top)
        goto draw_lower;

    /* must enclose this below section in brackets, else clang whines about 'goto' above */
    {
        const float dupper = static_cast<float>(x_mid_sub_top) / static_cast<float>(y_mid_sub_top);
        // TRIDBGMSG("UPPER:  dupper = %f\n", dupper);
        // draw upper sub-triangle
        for (int yi = topy; yi < std::min(midy, canvas.height()); yi++) {
            // drawHorizLine(static_cast<int>(sx0), yi, static_cast<int>(sx1), color);

            // BIG TODO: barycentric-aware pixel-plopping w/ mesh uv+texture info, if applicable

            sx0 += dupper;
            sx1 += dlong;
        }
    }

    // draw lower sub-triangle
draw_lower:
    if (0 == y_btm_sub_mid) {
        // TRIDBGMSG("Tri OK(U)\n");
        return; // no need to draw lower sub-triangle
    }

    const float dlower = static_cast<float>(x_btm_sub_mid) / static_cast<float>(y_btm_sub_mid);
    // TRIDBGMSG("LOWER:  dlower = %f\n", dlower);
    sx0 = static_cast<float>(midx);
    for (int yi = midy; yi < btmy; yi++) {
        // drawHorizLine(static_cast<int>(sx0), yi, static_cast<int>(sx1), color);

        // BIG TODO: barycentric-aware pixel-plopping w/ mesh uv+texture info, if applicable

        sx0 += dlower;
        sx1 += dlong;
    }

    // TRIDBGMSG("Tri OK\n");
}


void Render::drawMeshFace(Canvas& canvas,
                          const Camera& camera,
                          const Mesh& mesh,
                          int facenum,
                          const Vec3& v1, const Vec3& v2, const Vec3& v3, // world-space vertices
                          const Mat& tmat,
                          bool cullBackfaces)
{
    if ((facenum < 0) || (facenum > mesh.getFaces().size()))
        return;

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

    const std::function<float(float)> rfunc = ceilf;
    const int sx1 = static_cast<int>(rfunc((w * 0.5f) + (tv1.x * w * 0.5f / std::max(tv1.z, cnear)))),
              sy1 = static_cast<int>(rfunc((h * 0.5f) + (tv1.y * h * 0.5f / std::max(tv1.z, cnear)))),
              sx2 = static_cast<int>(rfunc((w * 0.5f) + (tv2.x * w * 0.5f / std::max(tv2.z, cnear)))),
              sy2 = static_cast<int>(rfunc((h * 0.5f) + (tv2.y * h * 0.5f / std::max(tv2.z, cnear)))),
              sx3 = static_cast<int>(rfunc((w * 0.5f) + (tv3.x * w * 0.5f / std::max(tv3.z, cnear)))),
              sy3 = static_cast<int>(rfunc((h * 0.5f) + (tv3.y * h * 0.5f / std::max(tv3.z, cnear))));

    const bool isFacingCamera = (!((sx1 < 0) && (sx1 > w) && (sx2 < 0) && (sx2 > w) && (sx3 < 0) && (sx3 > w) && (sy1 < 0) && (sy1 > h) && (sy2 < 0) && (sy2 > h) && (sy3 < 0) && (sy3 > h)));
    if (isFacingCamera)
        drawMeshTriangle(canvas, mesh, facenum, sx1, sy1, sx2, sy2, sx3, sy3);
}

void Render::drawMesh(Canvas& canvas, const Camera& camera, const Mesh& mesh)
{
    const Mat worldMatrix = camera.getProjMatrix() * camera.getViewMatrix();
    mesh.sortMeshTriangleDrawOrderFromCamera(worldMatrix, camera);

    for (const auto faceNum : mesh.getFaceSortBuffer()) {
        // another less important but still necessary TODO: materials!
        const byte color = 2 + (faceNum % 30);
        Vec3 v1, v2, v3;
        mesh.getMeshFaceVertices(faceNum, v1, v2, v3);

        // BIG TODO: replace with drawMeshFace(...)
        drawFlat3DTriangle(canvas, camera, color, v1, v2, v3, worldMatrix);
    }
}

