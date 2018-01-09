#pragma once

#include "Camera.hpp"
#include "CanvasDef.hpp"
#include "Mat.hpp"
#include "Mesh.hpp"
#include "Vec.hpp"

void drawFlat3DTriangle(Canvas& canvas, byte color, const Vec3& v1, const Vec3& v2, const Vec3& v3, const Mat& tmat);
void drawMesh(Canvas& canvas, const Camera& camera, const Mesh& mesh);