#include "Mesh.hpp"
#include "boost/tokenizer.hpp"
#include <fstream>
#include <sstream>
#include <string>

Mesh Mesh::loadMesh(std::string filename)
{
    std::vector<Vec3> vertexdata;
    std::vector<mesh_face_t> facedata;
    std::vector<mesh_face_uv_t> faceuvdata;
    std::vector<Vec2> uvdata;
    std::vector<Vec2> normaldata;

    std::ifstream infile(filename);
    std::string line;
    int vertn = 0, facen = 0, uvn = 0;

    while (std::getline(infile, line)) {
        std::vector<std::string> tokens;

        typedef boost::tokenizer<boost::char_separator<char>> tokenizer;
        boost::char_separator<char> sep(" ");
        tokenizer tok(line, sep);

        for (auto beg = tok.begin(); beg != tok.end(); ++beg) {
            tokens.push_back(*beg);
        }

        const auto directive = tokens[0];
        if (directive == "v") {
            const float x = std::stof(tokens[1].c_str());
            const float y = std::stof(tokens[2].c_str());
            const float z = std::stof(tokens[3].c_str());
            printf("Vertex %d: %f, %f, %f\n", vertn, x, y, z);
            vertexdata.push_back(Vec3(x, y, z));
            vertn++;
        } else if (directive == "vt") {
            const float u = std::stof(tokens[1].c_str());
            const float v = 1.0f - std::stof(tokens[2].c_str());
            uvdata.push_back(Vec2(u, v));
            printf("UV %d: %f, %f\n", uvn, u, v);
            uvn++;
        } else if (directive == "f") {
            std::string fparams[3] = { tokens[1], tokens[2], tokens[3] };

            bool f_has_uvs = false; // seems inelegant but whatever
            int fv[3];
            int fuv[3];
            // Two possible formats for 'f' directive may be seen:
            //   f vertIDX_1 vertIDX_2 vertIDX_3
            //   f vertIDX_1/uvIDX_1/normalIDX_1 vertIDX_2/uvIDX_2/normalIDX_2 vertIDX_3/uvIDX_3/normalIDX_3

            // for EACH PARAMETER...
            int i = 0;
            for (const auto& fparam : fparams) {
                typedef boost::tokenizer<boost::char_separator<char>> f_tokenizer;
                const boost::char_separator<char> f_sep("/");
                f_tokenizer f_tok(fparam, f_sep);
                std::vector<std::string> fptokens;
                for (auto beg = f_tok.begin(); beg != f_tok.end(); ++beg) {
                    printf("pushing %s\n", (*beg).c_str());
                    fptokens.push_back(*beg);
                }

                fv[i] = std::stoi(fptokens[0].c_str()) - 1;
                printf("fptokens size = %d\n", fptokens.size());

                switch (fptokens.size()) {
                case 1:
                    break; // OK
                case 3: // we don't care
                    f_has_uvs = true;
                    fuv[i] = std::stoi(fptokens[1].c_str()) - 1;
                    break;
                default:
                    throw "malformatted .obj";
                }

                i++;
            }

            printf("Face %d: %d, %d, %d\n", facen, fv[0], fv[1], fv[2]);
            facedata.push_back(mesh_face_t(fv[0], fv[1], fv[2]));
            if (f_has_uvs)
                faceuvdata.push_back(mesh_face_uv_t(fuv[0], fuv[1], fuv[2]));
            facen++;
        } else {
            printf("Unknown/Unsupported directive '%s'\n", directive.c_str());
        }
    }

    return Mesh(vertexdata, facedata, uvdata, faceuvdata);
}
