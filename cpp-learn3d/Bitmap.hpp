#pragma once

#include "canvas8.hpp"
#include <cstdio>
#include <cstring>
#include <string>

// #define _CRT_SECURE_NO_WARNINGS 1

class Bitmap {
public:
    Bitmap(int width, int height, const char* const filename)
        : _pixels(new byte[width * height])
        , _width(width)
        , _height(height)
    {
        if (nullptr == _pixels)
            throw "Failed to allocate pixels.";

        if (filename) {
            std::string fn = "gfx/" + std::string(filename);
            FILE* fp = fopen(fn.c_str(), "r");
            if (fp) {
                fread(_pixels, 1, width * height, fp);
                fclose(fp);

                for (int i = 0; i < width * height; i++)
                    if (0 == _pixels[i]) {
                        _hasZeroPixel = true;
                        break;
                    }
            } else {
                printf("File not found.\n");
                throw "File not found.";
            }
        } else {
            memset(_pixels, 0x00, width * height);
            _hasZeroPixel = true;
        }
    }

    Bitmap(int width, int height, byte* const memloc, bool copyPixels = false)
        : _pixels((copyPixels) ? new byte[width * height] : memloc)
        , _width(width)
        , _height(height)
    {
        // printf("initializing Bitmap: [%p --> %p]\n", memloc, _pixels);
        if (_pixels && copyPixels) {
            //			printf("copying from memloc %p into new memloc %p\n", memloc);
            memcpy(_pixels, memloc, width * height);
        }

        _werePixelsCopied = copyPixels;
    }

    //! Copy ctor
    Bitmap(const Bitmap& other)
        : _pixels(new byte[other._width * other._height])
        , _width(other._width)
        , _height(other._height)
    {
        memcpy(_pixels, other._pixels, _width * _height);
    }

    ~Bitmap()
    {
        if (_pixels && _werePixelsCopied) // only delete if ctor alloc'd, duh
            delete[] _pixels;
    }

    byte pixelAt(int x, int y) const { return _pixels[(y * _width) + x]; }
    byte* pixelPtrAt(int x, int y) const { return &_pixels[(y * _width) + x]; }

    unsigned int width() const { return _width; }
    unsigned int height() const { return _height; }

private:
    byte* const _pixels;
    const int _width;
    const int _height;

    bool _werePixelsCopied = false; /// did ctor alloc new memory, or is it using pre-existing chunk for pixel data?
    bool _hasZeroPixel = false; /// for optimization; if false, blitter uses faster memcpy-based routine
};
