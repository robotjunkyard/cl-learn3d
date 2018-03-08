#pragma once

#include "Camera.hpp"
#include "CanvasDef.hpp"
#include "Mat.hpp"
#include "Mesh.hpp"
#include "Vec.hpp"

class Render {
public:
    static void drawFlat3DTriangle(Canvas& canvas, const Camera& camera, byte color,
        const Vec3& v1, const Vec3& v2, const Vec3& v3,
        const Mat& tmat,
        bool cullBackfaces);

    static void drawMeshFlat(Canvas& canvas, const Camera& camera, const Mesh& mesh);     // flat-shaded debug mesh draw
    static void drawMeshTextured(Canvas& canvas, const Camera& camera, const Mesh& mesh); // textured mesh draw

    static float drawSubtriangleTextured(Canvas& canvas,
                                        float start_sx0, float start_sx1,
                                        float dsx0, // dupper, // dsx0
                                        float dsx1, // dlong,  // dsx1
                                        int yi_start,   // yi_start
                                        int yi_end,     // std::min(midy, h),  // yi_end
                                        const Bitmap& bitmap,
                                        const Triangle2& screenTri,
                                        const Triangle2& uvtri);
    static void drawMeshTriangleTextured (Canvas& canvas, const Mesh& mesh, unsigned short facenum,
                                      const Triangle2& uvtri,
                                      int x1, int y1, int x2, int y2, int x3, int y3);
    static void drawTexturedMeshFace(Canvas& canvas, const Mesh& mesh, const Camera& camera,
                                     const Mat& tmat,
                                     unsigned short facenum,
                                     const Triangle3& faceTri, const Triangle2& uvTri,
                                     bool cullBackfaces);
};
