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

    static void drawMeshTriangle(Canvas& canvas, const Mesh& mesh, int facenum,
                                 int x1, int y1, int x2, int y2, int x3, int y3);
    static void drawMeshFace(Canvas& canvas,
                             const Camera& camera,
                             const Mesh& mesh,
                             int facenum,
                             const Vec3& v1, const Vec3& v2, const Vec3& v3, // world-space vertices
                             const Mat& tmat,
                             bool cullBackfaces = true);

    /* static void drawMeshTriangle(Canvas& canvas, const Camera& camera, const Mesh& mesh,
                              const Vec3& v1, const Vec3& v2, const Vec3& v3, // world-space vertices
                              const Mat& tmat, bool cullBackfaces = true);*/

    static void drawMesh(Canvas& canvas, const Camera& camera, const Mesh& mesh);         // textured mesh draw
};
