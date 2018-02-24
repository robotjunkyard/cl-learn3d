// CanvasDef.hpp - Feb 2018 revision

#pragma once
#include "SDL2/SDL.h"
#include <algorithm>
#include <vector>
#include <array>

/*  #include "Bitmap.hpp"  */
#include "canvas8.hpp"

struct SDL_Texture;

struct color_t {
    byte r = 0, g = 0, b = 0, a = 255;

    color_t() {}
    color_t(byte __r, byte __g, byte __b, byte __a)
        : r(__r)
        , g(__g)
        , b(__b)
        , a(__a)
    {
    }

    ~color_t() {}

    uint32_t as_uint32() const
    {
        return *reinterpret_cast<uint32_t*>(const_cast<color_t*>(this));
    }
};

class Palette {
public:
    Palette(const std::array<color_t, 256>& colors)
        : _colors(colors)
        , _initColors(colors)
    {
        swapRedBlue();
    }

    ~Palette()
    {
    }

    void setColor(byte index, byte r, byte g, byte b, byte a)
    {
        color_t& c = _colors[index];
        c.r = r;
        c.g = g;
        c.b = b;
        c.a = a;
    }

    const color_t& getColor(int index) const
    {
        return _colors.at(index);
    }

    uint32_t getColorUINT32(byte index) const
    {
        return (_colors[index].as_uint32());
    }

private:
    void swapRedBlue(); // necessary because of SDL
    std::array<color_t, 256> _colors, _initColors;
};

class Canvas;

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
        , m_palette(palette)
    {
        if (!((width > 0) && (height > 0)))
            throw std::bad_alloc(); // bad_array_new_length();

        if (m_pixels.size() == 0)
            throw std::bad_alloc();
        if (m_destPixels32.size() == 0)
            throw std::bad_alloc();
    }

    ~Canvas()
    {    }

    void setPixel(int x, int y, byte color)
    {
        m_pixels[(y * width()) + x] = color;
    }

    void clear(byte color = 1)
    {
        for (auto& i : m_pixels)
            i = color;
    }

    byte getPixel(int x, int y) const
    {
        return m_pixels[(y * width()) + x];
    }

    void updateSDLTexture(SDL_Texture* sdlTexture) const;

    #ifdef BITMAP_HPP_INCLUDED
    void blitBitmap(const Bitmap& source, int destx, int desty, bool transparent = true)
    {
        if (transparent)
            blitBitmapMasked(source, destx, desty);
        else
            blitBitmapNonmasked(source, destx, desty);
    }
    #endif

    void drawHorizLine(int x1, int y, int x2, byte color);
    void drawRect(int x1, int y1, int x2, int y2, byte color);
    void drawTriangle(int x1, int y1, int x2, int y2, int x3, int y3, byte color);

    int width() const
    {
        return m_width;
    }
    int height() const
    {
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
