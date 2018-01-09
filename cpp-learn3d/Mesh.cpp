#include "Mesh.hpp"
#include "boost/tokenizer.hpp"
#include <fstream>
#include <sstream>
#include <string>

Mesh Mesh::loadMesh(std::string filename)
{
    std::vector<Vec3> vertexdata;
    std::vector<mesh_face_t> facedata;

    std::ifstream infile(filename);
    std::string line;
    int vertn = 0, facen = 0;

    while (std::getline(infile, line)) {
        int tvi1 = -1, tvi2 = -1, tvi3 = -1; // used if 'f' directive seen, tvi = "triangle's vertex index"
        float x = -1.0f, y = -1.0f, z = -1.0f; // used if 'v' directive seen, indicating a vertex

        std::vector<std::string> tokens;

        typedef boost::tokenizer<boost::char_separator<char>> tokenizer;
        boost::char_separator<char> sep(" ");
        tokenizer tok(line, sep);

        for (auto beg = tok.begin(); beg != tok.end(); ++beg) {
            tokens.push_back(*beg);
        }

        const auto directive = tokens[0];
        if (directive == "v") {
            x = std::stof(tokens[1].c_str());
            y = std::stof(tokens[2].c_str());
            z = std::stof(tokens[3].c_str());
            printf("Vertex %d: %f, %f, %f\n", vertn, x, y, z);
            vertexdata.push_back(Vec3(x, y, z));
            vertn++;
        } else if (directive == "f") {
            tvi1 = std::stoi(tokens[1].c_str()) - 1;
            tvi2 = std::stoi(tokens[2].c_str()) - 1;
            tvi3 = std::stoi(tokens[3].c_str()) - 1;
            printf("Face %d: %d, %d, %d\n", facen, tvi1, tvi2, tvi3);
            facedata.push_back(mesh_face_t(tvi1, tvi2, tvi3));
            facen++;
        } else {
            printf("Unknown/Unsupported directive '%s'\n", directive.c_str());
        }
    }

    return Mesh(vertexdata, facedata);
}
