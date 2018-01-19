#pragma once
#include "Vec.hpp"
#include <memory>
#include <vector>

class mesh_face_t {
public:
    mesh_face_t(int vi0, int vi1, int vi2)
        : v0(vi0)
        , v1(vi1)
        , v2(vi2)
    {
    }

    int getV0() const
    {
        return v0;
    }

    int getV1() const
    {
        return v1;
    }

    int getV2() const
    {
        return v2;
    }

private:
    const int v0, v1, v2;
};

class mesh_face_uv_t {
public:
    mesh_face_uv_t(int uvi0, int uvi1, int uvi2)
        : uv0(uvi0)
        , uv1(uvi1)
        , uv2(uvi2)
    {
    }

    int getUV0() const
    {
        return uv0;
    }

    int getUV1() const
    {
        return uv1;
    }

    int getUV2() const
    {
        return uv2;
    }

private:
    const int uv0, uv1, uv2;
};

class Mesh {
public:
    static Mesh loadMesh(std::string filename);
    const std::vector<mesh_face_t>& getFaces() const
    {
        return m_faces;
    }

    std::vector<unsigned int>& getFaceSortBuffer()
    {
        return m_facesortbuffer;
    }

    const std::vector<Vec3>& getVertices() const
    {
        return m_vertices;
    }

    void getMeshFaceVertices(const int faceIndex, Vec3& ov0, Vec3& ov1, Vec3& ov2) const
    {
        const auto face = m_faces[faceIndex];
        const int vidx0 = face.getV0(),
                  vidx1 = face.getV1(),
                  vidx2 = face.getV2();
        const Vec3 &v0 = m_vertices[vidx0],
                   &v1 = m_vertices[vidx1],
                   &v2 = m_vertices[vidx2];
        ov0 = v0;
        ov1 = v1;
        ov2 = v2;
    }

private:
    Mesh(const std::vector<Vec3>& vertexdata, const std::vector<mesh_face_t>& faceinfo)
        : m_vertices(vertexdata)
        , m_faces(faceinfo)
    {
        m_facesortbuffer.reserve(m_faces.size());
        for (unsigned int i = 0; i < m_faces.size(); i++)
            m_facesortbuffer.push_back(i);
    }

    Mesh(const std::vector<Vec3>& vertexdata, const std::vector<mesh_face_t>& faceinfo,
        const std::vector<Vec2>& uvdata, const std::vector<mesh_face_uv_t>& faceuvinfo)
        : m_vertices(vertexdata)
        , m_uvs(uvdata)
        , m_faces(faceinfo)
        , m_faces_uv(faceuvinfo)
    {
        m_facesortbuffer.reserve(m_faces.size());
        for (unsigned int i = 0; i < m_faces.size(); i++)
            m_facesortbuffer.push_back(i);
    }

    const std::vector<Vec3> m_vertices;
    const std::vector<Vec2> m_uvs; // optional
    const std::vector<mesh_face_t> m_faces;
    const std::vector<mesh_face_uv_t> m_faces_uv; // optional
    std::vector<unsigned int> m_facesortbuffer; // indices of faces, frequently re-sorted per frame
};
