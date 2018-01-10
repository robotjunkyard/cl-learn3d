#pragma once

#include "Mat.hpp"
#include "Vec.hpp"

class Camera {
private:
    Mat m_viewMatrix, m_projMatrix;
    Vec3 m_origin, m_target, m_up;
    float m_fov, m_near, m_far; // , _aspectratio;
    int m_xres, m_yres;

public:
    const Mat& lookAt(const Vec3& origin, const Vec3& target, const Vec3& up);

    // constructor will assume projection matrix being that of a perspective-type,
    // and that can always be changed later
    Camera(int xres, int yres,
        const Vec3& origin, const Vec3& target, const Vec3& up = Vec3(0.0f, 1.0f, 0.0f),
        float fov = 120.0f, float near = 0.5f, float far = 100.0f)
        : m_origin(origin)
        , m_target(target)
        , m_up(up)
        , m_fov(fov)
        , m_near(near)
        , m_far(far)
        , m_xres(xres)
        , m_yres(yres)
    {
        // float aspectratio = (float)xres / (float)yres;
        lookAt(m_origin, m_target, m_up);
        setPerspectiveProjection(fov, near, far);
    }

    const Mat& setPerspectiveProjection(float fov, float near, float far)
    {
        const float aspectratio = (float)m_xres / (float)m_yres;
        const float s = tan((fov / 2.0f) * (pi / 180.0f)); // remember pi * 180 converts (fov/2)° to radians

        /*const float j = (-near - far) / (far - near);
			   const float jj = 2.0f * far * near / (far - near);*/
        const float j = -far / (far - near);
        const float jj = -(far * near) / (far - near);

        m_projMatrix
            = Mat(1.0f / (s * aspectratio), 0.0f, 0.0f, 0.0f,
                0.0f, 1.0f / s, 0.0f, 0.0f,
                0.0f, 0.0f, j, 1.0,
                0.0f, 0.0f, jj, 0.0f);

        /*_projMatrix
				= Mat(1.0f, 0.0f, 0.0f, 0.0f,
				0.0f, 1.0f, 0.0f, 0.0f,
				0.0f, 0.0f, j, jj,
				0.0f, 0.0f, 1.0f, 0.0f);*/

        // interesting, this seems to work too.  I think I used this before in CL-LEARN3D and then
        // assumed I did something wrong when later seeing that EVERY OTHER MATH TEXT AND TUTORIAL
        // IN THE UNIVERSE used the other construct of this same matrix
        //
        // adapted from https://www.scratchapixel.com/lessons/3d-basic-rendering/perspective-and-orthographic-projection-matrix/building-basic-perspective-projection-matrix
        /* _projMatrix
            = Mat(s, 0.0f, 0.0f, 0.0f,
                0.0f, s * aspectratio, 0.0f, 0.0f,
                0.0f, 0.0f, j, 1.0,
                0.0f, 0.0f, jj, 0.0f);*/
        m_fov = fov;
        m_near = near;
        m_far = far;

        return m_projMatrix;
    }

    const Mat& getViewMatrix() const
    {
        return m_viewMatrix;
    }

    const Mat& getProjMatrix() const
    {
        return m_projMatrix;
    }

    const Vec3& getOrigin() const
    {
        return m_origin;
    }

    const Vec3& getTarget() const
    {
        return m_target;
    }

    const Vec3& getUp() const
    {
        return m_up;
    }

    float getNear() const
    {
        return m_near;
    }

    float getFar() const
    {
        return m_far;
    }

    float getFOV() const
    {
        return m_fov;
    }
};