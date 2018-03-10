// CanvasDef.hpp - Feb 2018 revision

#pragma once
#include "SDL2/SDL.h"
#include <algorithm>
#include <vector>
#include <array>

#include "Bitmap.hpp"
#include "canvas8.hpp"
#include "Triangle.hpp"

struct SDL_Texture;

struct color_t {
    byte r = 0, g = 0, b = 0, a = 255;

    color_t() = default;

    color_t(byte __r, byte __g, byte __b, byte __a)
        : r(__r)
        , g(__g)
        , b(__b)
        , a(__a) {
    }

    ~color_t() = default;

    uint32_t as_uint32() const {
        // return *reinterpret_cast<uint32_t*>(const_cast<color_t*>(this));
        return (a << 24) | (b << 16) | (g << 8) | r;
    }
};

class Palette {
public:
    Palette(const std::array<color_t, 256>& colors)
        : _initColors(withRedBlueSwapped(colors))
        , _colors(withRedBlueSwapped(colors)) {
    }

    ~Palette() {
    }

    void setColor(byte index, byte r, byte g, byte b, byte a) {
        color_t& c = _colors[index];
        c.r = r;
        c.g = g;
        c.b = b;
        c.a = a;
    }

    const color_t& getColor(byte index) const {
        return _colors.at(index);
    }

    uint32_t getColorUINT32(byte index) const {
        return (_colors[index].as_uint32());
    }

    //! Find an indexed color closest to the specified arbitrary one, returning the index
    std::pair<const color_t&, byte> findApproximateColor(color_t color) const;

private:
    std::array<color_t, 256> withRedBlueSwapped(const std::array<color_t, 256>& other) {
        std::array<color_t, 256> newcolors = other;
        for (auto& color : newcolors)
            std::swap(color.r, color.b);

        return newcolors;
    }

    const std::array<color_t, 256> _initColors;
    std::array<color_t, 256> _colors;
};

/** Canvas is the game's internal representation of a virtual 8-bit display.
	This is because SDL2 was giving me a bunch of shit about not allowing
	palettized textures, even though it's in the SDL2 API. */
class Canvas {
public:
    Canvas(Palette& palette, int width, int height)
        : m_pixels(std::vector<byte>(std::max(0, width) * std::max(0, height)))
        , m_destPixels32(std::vector<uint32_t>(std::max(0, width) * std::max(0, height)))
        , m_width(width)
        , m_height(height)
        , m_palette(palette) {
        if (!((width > 0) && (height > 0)))
            throw std::bad_alloc(); // bad_array_new_length();

        if (m_pixels.size() == 0)
            throw std::bad_alloc();
        if (m_destPixels32.size() == 0)
            throw std::bad_alloc();
    }

    ~Canvas() = default;

    void setPixel(int x, int y, byte color) {
        m_pixels[(y * width()) + x] = color;
    }

    void clear(byte color = 1) {
        for (auto& i : m_pixels)
            i = color;
    }

    byte getPixel(int x, int y) const {
        return m_pixels[(y * width()) + x];
    }

    void updateSDLTexture(SDL_Texture* sdlTexture) const;

#ifdef BITMAP_HPP_INCLUDED
    void blitBitmap(const Bitmap& source, int destx, int desty, bool transparent = true) {
        if (transparent)
            blitBitmapMasked(source, destx, desty);
        else
            blitBitmapNonmasked(source, destx, desty);
    }
#endif

    void drawHorizLine(int x1, int y, int x2, byte color);
    void drawRect(int x1, int y1, int x2, int y2, byte color);
    void drawFlatTriangle(int x1, int y1, int x2, int y2, int x3, int y3, byte color);
    void drawFlatTriangle(const Triangle2& tri, byte color) {
        const auto& a = tri.a,
                    b = tri.b,
                    c = tri.c;
        drawFlatTriangle(static_cast<int>(a.x), static_cast<int>(a.y),
                         static_cast<int>(b.x), static_cast<int>(b.y),
                         static_cast<int>(c.x), static_cast<int>(c.y),
                         color);
    }

    int width() const {
        return m_width;
    }

    int height() const {
        return m_height;
    }

private:
#ifdef BITMAP_HPP_INCLUDED
    void blitBitmapMasked(const Bitmap& source, int destx, int desty);
    void blitBitmapNonmasked(const Bitmap& source, int destx, int desty);
#endif // BITMAP_HPP_INCLUDED

    std::vector<byte> m_pixels;
    mutable std::vector<uint32_t> m_destPixels32;  // intermediate 8->32->SDL scratchpad buffer
    int m_width, m_height;
    Palette m_palette;
};
