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
    mesh.sortMeshTriangleDrawOrderFromCamera(/* worldMatrix, */ camera);

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

// returns where sx1 (long) left off
//
// BIG OPTIMIZATION TODO: this needlessly goes through loop iterations for off-screen rows.
//   Attempts at implementing this optimization with what seemed to be obvious and simple arithmetic have
//   failed so far, with mangled-looking triangles... sigh.
float Render::drawSubtriangleTextured(Canvas& canvas,
    float start_sx0,
    float start_sx1, // for the "long" edge of greater triangle
    float dsx0,
    float dsx1, // delta of "long" edge of greater triangle
    int yi_start, // starting scanline
    int yi_end, // end scanline
    const Bitmap& bitmap, // texture
    const Triangle2& screenTri,
    const Triangle2& uvtri)
{
    const auto tex_w = bitmap.width(),
               tex_h = bitmap.height(),
               can_w = canvas.width(),
               can_h = canvas.height(),
               yiend = std::min(can_h, yi_end);
    auto sx0 = start_sx0,
         sx1 = start_sx1;

    if (yi_start > can_h)
        return sx1;

    // for each row...
    for (int yi = yi_start; yi < yiend; yi++) {
        // and for each pixel in that row...
        if (yi >= 0) // && yi < yiend)
            for (int xi = std::min(static_cast<int>(sx0), static_cast<int>(sx1));
                 xi < std::max(static_cast<int>(sx0), static_cast<int>(sx1));
                 xi++) {
                if ((xi >= 0) && (xi < can_w)) {
                    const Vec2 pixelvec = { static_cast<float>(xi), static_cast<float>(yi) };
                    const Vec3 baryc = screenTri.barycentricCoordinates(pixelvec);
                    const Vec2 cartuv = uvtri.pointFromBarycentric(baryc);

                    /*const int src_uv_x = modu(static_cast<int>(tex_w * cartuv.x), tex_w),
                              src_uv_y = modu(static_cast<int>(tex_h * cartuv.y), tex_h);*/

                    // OPTIMIZATION: right now, textures are strictly 256x256, so instead of using modulus
                    // (which = division, eww) to "wrap around" a texture, just let the 8-bit integer overflow!
                    // And this can be updated later to allow any 2^n bitmap texture, using simple bitmask!
                    const std::uint8_t src_uv_x = static_cast<std::uint8_t>(tex_w * cartuv.x),
                                       src_uv_y = static_cast<std::uint8_t>(tex_h * cartuv.y);

                    const auto diffuse_color = bitmap.pixelAt(src_uv_x, src_uv_y);

                    canvas.setPixel(xi, yi, diffuse_color);
                }
            }

        sx0 += dsx0;
        sx1 += dsx1;
    }

    return sx1;
}

void Render::drawMeshTriangleTextured(Canvas& canvas, const Mesh& mesh,
    const Triangle2& uvtri,
    int x1, int y1, int x2, int y2, int x3, int y3)
{
    // previous functions leading up to this one should have already
    // assured a bitmap exists
    const Bitmap& bitmap = *mesh.getTexture();
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
    float sx1 = static_cast<float>(topx);

    // draw upper subtriangle, if applicable
    if (0 != y_mid_sub_top)
        sx1 = drawSubtriangleTextured(canvas,
            sx1, sx1,
            static_cast<float>(x_mid_sub_top) / static_cast<float>(y_mid_sub_top), // dsx0
            dlong, // dsx1
            topy, // yi_start
            midy, // std::min(midy, h),  // yi_end
            bitmap, screenTri, uvtri);

    // draw lower subtriangle, if applicable
    if (0 != y_btm_sub_mid)
        drawSubtriangleTextured(canvas,
            static_cast<float>(midx), sx1,
            static_cast<float>(x_btm_sub_mid) / static_cast<float>(y_btm_sub_mid), // dsx0
            dlong, // dsx1
            midy, // yi_start
            btmy, // std::min(btmy, h),  // yi_end
            bitmap, screenTri, uvtri);
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

    const std::function<float(float)> rfunc = rintf;
    const int sx1 = static_cast<int>(rfunc((w * 0.5f) + (tv1.x * w * 0.5f / std::max(tv1.z, cnear)))),
              sy1 = static_cast<int>(rfunc((h * 0.5f) + (tv1.y * h * 0.5f / std::max(tv1.z, cnear)))),
              sx2 = static_cast<int>(rfunc((w * 0.5f) + (tv2.x * w * 0.5f / std::max(tv2.z, cnear)))),
              sy2 = static_cast<int>(rfunc((h * 0.5f) + (tv2.y * h * 0.5f / std::max(tv2.z, cnear)))),
              sx3 = static_cast<int>(rfunc((w * 0.5f) + (tv3.x * w * 0.5f / std::max(tv3.z, cnear)))),
              sy3 = static_cast<int>(rfunc((h * 0.5f) + (tv3.y * h * 0.5f / std::max(tv3.z, cnear))));

    drawMeshTriangleTextured(canvas, mesh,
        uvTri,
        sx1, sy1, sx2, sy2, sx3, sy3);
}

void Render::drawMeshTextured(Canvas& canvas, const Camera& camera, const Mesh& mesh)
{
    if (!mesh.getTexture())
        drawMeshFlat(canvas, camera, mesh);

    const Mat worldMatrix = camera.getProjMatrix() * camera.getViewMatrix();
    mesh.sortMeshTriangleDrawOrderFromCamera(/* worldMatrix, */ camera);

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
