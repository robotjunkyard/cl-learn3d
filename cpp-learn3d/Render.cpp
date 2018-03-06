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

//! draws a mesh, flat-shaded (does not yet honor or support .obj materials,
//! so colors are arbitrary "debug" style appearance)
void Render::drawMeshFlat(Canvas& canvas, const Camera& camera, const Mesh& mesh)
{
    const Mat worldMatrix = camera.getProjMatrix() * camera.getViewMatrix();
    mesh.sortMeshTriangleDrawOrderFromCamera(worldMatrix, camera);

    for (const auto faceNum : mesh.getFaceSortBuffer()) {
        const byte color = 2 + (faceNum % 30);
        const Triangle3 tri = mesh.getMeshFaceVertices(faceNum);
        drawFlat3DTriangle(canvas, camera, color, tri.a, tri.b, tri.c, worldMatrix);
    }
}

inline int modu(int x, int y)
{
    return x % y + (x % y < 0 ? y : 0);
}

void Render::drawMeshTriangleTextured(Canvas& canvas, const Mesh& mesh, unsigned short facenum,
                                      const Triangle2& uvtri,
                                      int x1, int y1, int x2, int y2, int x3, int y3)
{
    // previous functions leading up to this one should have already
    // assured a bitmap exists
    const Bitmap* const bitmap = mesh.getTexture();
    const auto tex_w = bitmap->width(),
                       tex_h = bitmap->height();
    const Triangle2 screenTri = Triangle2(x1, y1, x2, y2, x3, y3);
    const int h = canvas.height();

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

    if ((0 == y_btm_sub_top) || (btmy < 0) || (topy >= h))
        return;

    const float dlong = static_cast<float>(x_btm_sub_top) / static_cast<float>(y_btm_sub_top);
    float sx0 = static_cast<float>(topx),
          sx1 = sx0;

    if (0 == y_mid_sub_top)
        goto draw_lower;

    /* must enclose this below section in brackets, else clang whines about 'goto' above */
    {
        const float dupper = static_cast<float>(x_mid_sub_top) / static_cast<float>(y_mid_sub_top);
        // TRIDBGMSG("UPPER:  dupper = %f\n", dupper);
        //  --- draw upper sub-triangle ---

        // for each row...
        for (int yi = topy,
                 yiend = std::min(midy, h);
             yi < yiend;
             yi++)
        {
            // and for each pixel in that row...
            if (yi >= 0 && yi < midy)
                for (int xi = std::min(sx0, sx1);  // static_cast<int>(std::max(0.0f, sx0));
                         xi < std::max(sx0, sx1);  // std::min(static_cast<float>(canvas.width()), sx1);
                         xi++)
                {
                    const Vec2 pixelvec = { static_cast<float>(xi), static_cast<float>(yi) };
                    const Vec3 baryc = screenTri.barycentricCoordinates(pixelvec);
                    const Vec2 cartuv = uvtri.pointFromBarycentric(baryc);

                    const int src_uv_x = modu(static_cast<int>(tex_w * cartuv.x), tex_w),
                              src_uv_y = modu(static_cast<int>(tex_h * cartuv.y), tex_h);

                    const auto color = bitmap->pixelAt(src_uv_x, src_uv_y);
                    canvas.setPixel(xi, yi, color);
                }

            sx0 += dupper;
            sx1 += dlong;
        }
    }

    // do same but to draw lower sub-triangle
draw_lower:
    if (0 == y_btm_sub_mid) {
        // TRIDBGMSG("Tri OK(U)\n");
        return; // no need to draw lower sub-triangle
    }

    const float dlower = static_cast<float>(x_btm_sub_mid) / static_cast<float>(y_btm_sub_mid);
    // TRIDBGMSG("LOWER:  dlower = %f\n", dlower);
    sx0 = static_cast<float>(midx);

    // for each row...
    for (int yi = midy,
             yiend = std::min(btmy, h);
             yi < yiend;
             yi++) {
        // and for each pixel in that row...
        if (yi >= 0 && yi < btmy)
        {
            for (int xi = std::min(static_cast<int>(sx0), static_cast<int>(sx1));
                     xi < std::max(static_cast<int>(sx0), static_cast<int>(sx1));
                     xi++)
            {
                const Vec2 pixelvec = { static_cast<float>(xi), static_cast<float>(yi) };
                const Vec3 baryc = screenTri.barycentricCoordinates(pixelvec);
                const Vec2 cartuv = uvtri.pointFromBarycentric(baryc);

                const int src_uv_x = modu(static_cast<int>(tex_w * cartuv.x), tex_w),
                          src_uv_y = modu(static_cast<int>(tex_h * cartuv.y), tex_h);

                const auto color = bitmap->pixelAt(src_uv_x, src_uv_y);
                canvas.setPixel(xi, yi, color);
            }
        }

        sx0 += dlower;
        sx1 += dlong;
    }

    // TRIDBGMSG("Tri OK\n");
}


void Render::drawTexturedMeshFace(Canvas& canvas, const Mesh& mesh, const Camera& camera,
                                  const Mat& tmat, unsigned short facenum,
                                  const Triangle3& faceTri, const Triangle2& uvTri,
                                  bool cullBackfaces)
{
    if (facenum > mesh.getFaces().size())
        return;

    // Perf TODO: probably better to do this in one clustered batch for all world geometry
    // to a buffer full of screen-space triangles, and then actually draw from that buffer
    const Vec3 tv1 = tmat * faceTri.a,
               tv2 = tmat * faceTri.b,
               tv3 = tmat * faceTri.c,
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

    //const bool isFacingCamera = (!((sx1 < 0) && (sx1 > w) && (sx2 < 0) && (sx2 > w) && (sx3 < 0) && (sx3 > w) && (sy1 < 0) && (sy1 > h) && (sy2 < 0) && (sy2 > h) && (sy3 < 0) && (sy3 > h)));
    //if (isFacingCamera)
    drawMeshTriangleTextured(canvas, mesh, facenum,
                             uvTri,
                             sx1, sy1, sx2, sy2, sx3, sy3);
}

void Render::drawMeshTextured(Canvas& canvas, const Camera& camera, const Mesh& mesh)
{
    if (!mesh.getTexture())
        drawMeshFlat(canvas, camera, mesh);

    const Mat worldMatrix = camera.getProjMatrix() * camera.getViewMatrix();
    mesh.sortMeshTriangleDrawOrderFromCamera(worldMatrix, camera);

    for (const auto facenum : mesh.getFaceSortBuffer()) {
        // another less important but still necessary TODO: materials!
        const Triangle3 tri = mesh.getMeshFaceVertices(facenum);
        const Triangle2 uvtri = mesh.getMeshUVCoords(facenum);
        drawTexturedMeshFace(canvas, mesh, camera,
                             worldMatrix,
                             facenum,
                             tri, uvtri,
                             true);
    }
}

