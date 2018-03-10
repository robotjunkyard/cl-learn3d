#include "Mat.hpp"
#include <cstdio>

void Mat::print() const {
    printf(" |  %.2f  %.2f  %.2f  %.2f  |\n", m11, m12, m13, m14);
    printf(" |  %.2f  %.2f  %.2f  %.2f  |\n", m21, m22, m23, m24);
    printf(" |  %.2f  %.2f  %.2f  %.2f  |\n", m31, m32, m33, m34);
    printf(" |  %.2f  %.2f  %.2f  %.2f  |\n", m41, m42, m43, m44);
}
