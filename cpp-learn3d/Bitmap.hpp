#pragma once

#define BITMAP_HPP_INCLUDED

#include "canvas8.hpp"
#include <cstdio>
#include <cstring>
#include <string>

/// A BitMap is an 8-bit indexed color image.
///
/// TODO: Whip up an ad-hoc file format later, one that has width and height in its header,
/// and write a little exporter for it for GIMP, so that the need to manually specify width
/// and height in the Bitmap constructor (eww) can be eliminated.
class Bitmap {
public:
    Bitmap(int width, int height, const std::string& filename, const std::string& directory = "gfx")
        : m_pixels(new byte[width * height])
        , m_width(width)
        , m_height(height) {
        if (nullptr == m_pixels)
            throw "Failed to allocate pixels.";

        const std::string fn = directory + "/" + filename;
        FILE* fp = fopen(fn.c_str(), "r");
        if (fp) {
            fread(m_pixels, 1, width * height, fp);
            fclose(fp);

            for (int i = 0; i < width * height; i++)
                if (0 == m_pixels[i]) {
                    m_hasZeroPixel = true;
                    break;
                }
        } else {
            printf("File not found.\n");
            memset(m_pixels, 0, width * height);
            throw "File not found.";
        }
    }

    Bitmap(int width, int height)
        : m_pixels(new byte[width * height])
        , m_width(width)
        , m_height(height) {
        if (nullptr == m_pixels)
            throw "Failed to allocate pixels.";

        memset(m_pixels, 0, width * height);
        m_hasZeroPixel = true;
    }

    Bitmap(int width, int height, byte* const memloc, bool copyPixels = false)
        : m_pixels((copyPixels) ? new byte[width * height] : memloc)
        , m_width(width)
        , m_height(height) {
        // printf("initializing Bitmap: [%p --> %p]\n", memloc, _pixels);
        if (m_pixels && copyPixels) {
            //			printf("copying from memloc %p into new memloc %p\n", memloc);
            memcpy(m_pixels, memloc, width * height);
        }

        m_werePixelsCopied = copyPixels;
    }

    Bitmap(const Bitmap& other)
        : Bitmap(other.m_width, other.m_height, other.m_pixels, true) {
    }

    ~Bitmap() {
        if (m_pixels && m_werePixelsCopied) // only delete if ctor alloc'd, duh
            delete[] m_pixels;
    }

    byte pixelAt(int x, int y) const {
        return m_pixels[(y * m_width) + x];
    }
    // byte* pixelPtrAt(int x, int y) { return &m_pixels[(y * m_width) + x]; }
    const byte* pixelPtrAt(int x, int y) const {
        return &m_pixels[(y * m_width) + x];
    }

    int width() const {
        return m_width;
    }
    int height() const {
        return m_height;
    }

private:
    byte* const m_pixels;
    const int m_width;
    const int m_height;

    bool m_werePixelsCopied = false; /// did ctor alloc new memory, or is it using pre-existing chunk for pixel data?
    bool m_hasZeroPixel = false; /// for optimization; if false, blitter uses faster memcpy-based routine
};
