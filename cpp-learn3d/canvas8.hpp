#pragma once
#include <algorithm>
#include <cstdint>
#include <set>

const int CANVAS_WIDTH = 480, //512,
    CANVAS_HEIGHT = 270; // 288
const int canvasToWindowScale = 2;

typedef uint8_t byte;
typedef std::pair<int, int> Point;

// confirmed this generates a nice 2-CPU-instruction-long thing (under Release build)
inline byte reduceToMask(byte val)
{
    return (val == 0) ? 0 : 0xFF;
}

inline byte reduceToInverseMask(byte val)
{
    return (val == 0) ? 0xFF : 0;
}

inline void swap1pair(int& a, int& b)
{
    const int tmp = a;
    a = b;
    b = tmp;
}

inline void swap2pair(int& a1, int& b1, int& a2, int& b2)
{
    int tmp = a1;
    a1 = b1;
    b1 = tmp;
    tmp = a2;
    a2 = b2;
    b2 = tmp;
}

inline int clamp(int val, int low, int high)
{
    return std::min(std::max(val, low), high);
}
