#include "Camera.hpp"

const Mat& Camera::lookAt(const Vec3& origin, const Vec3& target, const Vec3& up) {
    const Vec3 vz = (origin - target).normalize(),
               vx = Vec3::cross(up, vz).normalize(),
               vy = Vec3::cross(vz, vx);
    const float dotxin = (Vec3::dot(vx, -origin)),
                dotyin = (Vec3::dot(vy, -origin)),
                dotzin = (Vec3::dot(vz, -origin));
    m_viewMatrix = Mat(vx.x, vy.x, vz.x, 0.0,
                       vx.y, vy.y, vz.y, 0.0,
                       vx.z, vy.z, vz.z, 0.0,
                       dotxin, dotyin, dotzin, 1.0);

    m_origin = origin;
    m_target = target;
    m_up = up;

    return m_viewMatrix;
}
