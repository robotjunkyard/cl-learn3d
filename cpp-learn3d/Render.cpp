#include "Render.hpp"
#include "PredicateQuicksort.hpp"
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

    const int sx1 = static_cast<int>((w * 0.5f) + (tv1.x * w * 0.5f / std::max(tv1.z, cnear))),
              sy1 = static_cast<int>((h * 0.5f) + (tv1.y * h * 0.5f / std::max(tv1.z, cnear))),
              sx2 = static_cast<int>((w * 0.5f) + (tv2.x * w * 0.5f / std::max(tv2.z, cnear))),
              sy2 = static_cast<int>((h * 0.5f) + (tv2.y * h * 0.5f / std::max(tv2.z, cnear))),
              sx3 = static_cast<int>((w * 0.5f) + (tv3.x * w * 0.5f / std::max(tv3.z, cnear))),
              sy3 = static_cast<int>((h * 0.5f) + (tv3.y * h * 0.5f / std::max(tv3.z, cnear)));

    if (!((sx1 < 0) && (sx1 > w) && (sx2 < 0) && (sx2 > w) && (sx3 < 0) && (sx3 > w) && (sy1 < 0) && (sy1 > h) && (sy2 < 0) && (sy2 > h) && (sy3 < 0) && (sy3 > h)))
        canvas.drawTriangle(sx1, sy1, sx2, sy2, sx3, sy3, color);
}

/*void drawMeshOld(Canvas& canvas, const Camera& camera, const Mesh& mesh)
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
}*/

// comparative vertex distance means it is not the LITERAL distance between
// two vertices, but a value that's good enough for comparing two distances.
// Basically, this omits slower sqrt calculations which are unnecessary
// if you don't care about the ACTUAL distances--ONLY care about comparing them
float comparativeVertexDistance(const Vec3& a, const Vec3& b)
{
    const float axbx = a.x - b.x,
                ayby = a.y - b.y,
                azbz = a.z - b.z,
                result = (axbx * axbx) + (ayby * ayby) + (azbz * azbz);
    //printf("%f\n", result);
    return abs(result);
}

void sortMeshTriangles(Mesh& mesh, const Mat& tmat, const Camera& camera)
{
    // triangle sort valuator lambda
    auto triSortValuator = [&](int facenum) -> float {
        const Vec3& eye = camera.getOrigin();
        Vec3 v1, v2, v3;
        mesh.getMeshFaceVertices(facenum, v1, v2, v3);
        const Vec3 tv1 = tmat * v1,
                   tv2 = tmat * v2,
                   tv3 = tmat * v3,
                   tcp = tmat * -eye;
        // point whose X,Y,Z values are the MINIMUM values among each of the
        // X,Y,Z values of the three transformed points of the triangle.
        // TODO: not sure if this is always the ideal approach.  Models with
        // concavity tended to flicker in cl-learn3d.

        const Vec3 mintv
            = Vec3(std::min(tv1.x, std::min(tv2.x, tv3.x)),
                std::min(tv1.y, std::min(tv2.y, tv3.y)),
                std::min(tv1.z, std::min(tv2.z, tv3.z)));
        const Vec3 maxtv
            = Vec3(std::max(tv1.x, std::max(tv2.x, tv3.x)),
                std::max(tv1.y, std::max(tv2.y, tv3.y)),
                std::max(tv1.z, std::max(tv2.z, tv3.z)));
        const Vec3 avgtv
            = Vec3((tv1.x + tv2.x + tv3.x) / 3.0,
                (tv1.y + tv2.y + tv3.y) / 3.0,
                (tv1.z + tv2.z + tv3.z) / 3.0);
        return comparativeVertexDistance(tcp, maxtv);
    };

    pQuicksort<unsigned int, float>(mesh.getFaceSortBuffer(), 0, mesh.getFaceSortBuffer().size() - 1, triSortValuator);
}

void drawMesh(Canvas& canvas, const Camera& camera, Mesh& mesh)
{
    const Mat worldMatrix = camera.getProjMatrix() * camera.getViewMatrix(); // *Mat::identity(); // *Mat::scaleMatrix(0.02f);

    sortMeshTriangles(mesh, worldMatrix, camera);

    // **HUGE** TODO: re-impl. SORT-MESH-FACE-DRAW-ORDER, which in CL-LEARN3D
    // done w/ a customized version of quicksort that took a predicate
    // function.  Wonder if that can be just as elegantly done in C++14's
    // lambda function facilities, which I haven't yet messed w/ very much...
    for (const auto faceNum : (const_cast<Mesh&>(mesh)).getFaceSortBuffer()) {
        // another less important but still necessary TODO: materials!
        byte color = 2 + (faceNum % 30);
        Vec3 v1, v2, v3;
        mesh.getMeshFaceVertices(faceNum, v1, v2, v3);

        drawFlat3DTriangle(canvas, camera, color, v1, v2, v3, worldMatrix);
    }
}