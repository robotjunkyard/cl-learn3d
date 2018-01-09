#pragma once

#include "Mat.hpp"
#include "Vec.hpp"

class Camera {
private:
    Mat _viewMatrix, _projMatrix;
    Vec3 _origin, _target, _up;
    float _fov, _near, _far; // , _aspectratio;
    int _xres, _yres;

public:
    const Mat& lookAt(const Vec3& origin, const Vec3& target, const Vec3& up);

    // constructor will assume projection matrix being that of a perspective-type,
    // and that can always be changed later
    Camera(int xres, int yres,
        const Vec3& origin, const Vec3& target, const Vec3& up = Vec3(0.0f, 1.0f, 0.0f),
        float fov = 90.0f, float near = 0.5f, float far = 100.0f)
        : _origin(origin)
        , _target(target)
        , _up(up)
        , _fov(fov)
        , _near(near)
        , _far(far)
        , _xres(xres)
        , _yres(yres)
    {
        // float aspectratio = (float)xres / (float)yres;
        lookAt(_origin, _target, _up);
        setPerspectiveProjection(fov, near, far);
    }

    const Mat& setPerspectiveProjection(float fov, float near, float far)
    {
        float aspectratio = (float)_xres / (float)_yres;
        const float s = tan((fov / 2.0f) * (pi / 180.0f)); // remember pi * 180 converts (fov/2)° to radians
        const float j = (-(near + far)) / (far - near);
        const float jj = 2.0f * far * near / (far - near);

        _projMatrix
            = Mat(1.0f / (s * aspectratio), 0.0f, 0.0f, 0.0f,
                0.0f, 1.0f / s, 0.0f, 0.0f,
                0.0f, 0.0f, j, jj,
                0.0f, 0.0f, 1.0f, 0.0f);
        _fov = fov;
        _near = near;
        _far = far;

        return _projMatrix;
    }

    const Mat& getViewMatrix() const
    {
        return _viewMatrix;
    }

    const Mat& getProjMatrix() const
    {
        return _projMatrix;
    }

    const Vec3& getOrigin() const
    {
        return _origin;
    }

    const Vec3& getTarget() const
    {
        return _target;
    }

    const Vec3& getUp() const
    {
        return _up;
    }
};