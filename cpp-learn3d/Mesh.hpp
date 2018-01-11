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
    int v0, v1, v2;
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
        for (unsigned int i = 0; i < m_faces.size(); i++)
            m_facesortbuffer.push_back(i);
    }

    const std::vector<Vec3> m_vertices;
    const std::vector<mesh_face_t> m_faces;
    mutable std::vector<unsigned int> m_facesortbuffer; // indices of faces, frequently re-sorted per frame
};
