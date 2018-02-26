#pragma once

#include "PredicateQuicksort.hpp"
#include "Vec.hpp"
#include "Bitmap.hpp"
#include <memory>
#include <vector>
#include <numeric>

class Camera;
struct Mat;

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
    static Mesh loadMesh(const std::string& meshname);

    const std::vector<mesh_face_t>& getFaces() const
    {
        return m_faces;
    }

    const std::vector<unsigned int>& getFaceSortBuffer() const
    {
        return m_facesortbuffer;
    }

    const std::vector<Vec3>& getVertices() const
    {
        return m_vertices;
    }

    void getMeshFaceVertices(const int faceIndex, Vec3& ov0, Vec3& ov1, Vec3& ov2) const
    {
        const auto& face = m_faces[faceIndex];
        ov0 = m_vertices[face.getV0()];
        ov1 = m_vertices[face.getV1()];
        ov2 = m_vertices[face.getV2()];
    }

    const Bitmap* const getTexture() const
    {
        return m_texture.get();
    }

    // Sorts a mesh's draw-order buffer according to transformation matrix and camera orientation
    // This is const because it is not something that alters first-class data, only the draw order
    // which is considered highly mutable second-class data that is not specific to the information
    // about the model's actual geometry itself.
    //
    // This all said, there's probably a better way to go about this than having the draw-order-buffer
    // be a part of Mesh itself.  But that sort of re-org work comes much, much later...
    void sortMeshTriangleDrawOrderFromCamera(const Mat& tmat, const Camera& camera) const;

private:
    Mesh(const std::vector<Vec3>& vertexdata, const std::vector<mesh_face_t>& faceinfo)
        : m_vertices(vertexdata)
        , m_uvs({})
        , m_faces(faceinfo)
        , m_faces_uv({})
        , m_facesortbuffer(defaultFaceSortBuffer(m_faces.size()))
        , m_texture(nullptr)
    {
    }

    Mesh(const std::vector<Vec3>& vertexdata, const std::vector<mesh_face_t>& faceinfo,
         const std::vector<Vec2>& uvdata, const std::vector<mesh_face_uv_t>& faceuvinfo,
         const std::string& textureFilename)
        : m_vertices(vertexdata)
        , m_uvs(uvdata)
        , m_faces(faceinfo)
        , m_faces_uv(faceuvinfo)
        , m_facesortbuffer(defaultFaceSortBuffer(m_faces.size()))
        , m_texture(std::move(loadTexture(textureFilename)))
    {
    }

    static std::vector<unsigned int> defaultFaceSortBuffer(int count)
    {
        std::vector<unsigned int> buf(count);
        std::iota(buf.begin(), buf.end(), 0);  // fill with range 0..count
        return buf;
    }

    std::unique_ptr<Bitmap> loadTexture(const std::string& filename) const;

    const std::vector<Vec3> m_vertices;
    const std::vector<Vec2> m_uvs; // optional
    const std::vector<mesh_face_t> m_faces;
    const std::vector<mesh_face_uv_t> m_faces_uv; // optional

    // rendering-related scratchpad-ish data
    mutable std::vector<unsigned int> m_facesortbuffer; // indices of faces, frequently re-sorted per frame

    std::unique_ptr<Bitmap> m_texture;
};
