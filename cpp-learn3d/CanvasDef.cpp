#include "CanvasDef.hpp"
#include <tuple>
#include <utility>

#undef PRINT_TRIANGLE_DEBUG

#ifdef PRINT_TRIANGLE_DEBUG
#define TRIDBGMSG(fmt, ...) printf(fmt, __VA_ARGS__)
#else
#define TRIDBGMSG(...)
#endif

void Canvas::updateSDLTexture(SDL_Texture* sdlTexture) const
{
    for (int y = 0; y < height(); y++) {
        for (int x = 0; x < width(); x++) {
            const unsigned int idx = (width() * y) + x;
            const byte color8 = m_pixels[idx];
            const uint32_t truecolor = m_palette.getColor(color8).as_uint32();

            m_destPixels32[idx] = truecolor;
        }
    }

    SDL_UpdateTexture(sdlTexture, nullptr, m_destPixels32.data(), m_width * sizeof(m_destPixels32[0])); // oh!  pitch is BYTES, not PIXELS!  so... "* 4" !
}

#ifdef BITMAP_HPP_INCLUDED
void Canvas::blitBitmapMasked(const Bitmap& bitmap, int destx, int desty)
{
    // figure out clipping
    const int canvW = width(),
              canvH = height(),
              sourceWidth = bitmap.width(),
              sourceHeight = bitmap.height(),
              topSpriteCanvasX = destx,
              topSpriteCanvasY = desty,
              endSpriteCanvasX = destx + sourceWidth,
              endSpriteCanvasY = desty + sourceHeight,
              topSpriteClippedX = (topSpriteCanvasX < 0) ? 0 : topSpriteCanvasX,
              topSpriteClippedY = (topSpriteCanvasY < 0) ? 0 : topSpriteCanvasY,
              endSpriteClippedX = std::min<int>(canvW, endSpriteCanvasX),
              endSpriteClippedY = std::min<int>(canvH, endSpriteCanvasY),
              bdx = std::max<int>(destx, 0), // beginning X offset of dest canvas
        bdy = std::max<int>(desty, 0), // beginning Y offset of dest canvas
        axB = topSpriteClippedX - topSpriteCanvasX, // beginning X offset of source bitmap
        ayB = topSpriteClippedY - topSpriteCanvasY, // beginning Y offset of source bitmap
        s_across_x = endSpriteClippedX - topSpriteClippedX,
              s_across_y = endSpriteClippedY - topSpriteClippedY;

    for (int y = 0; y < s_across_y; y++) {
        for (int x = 0; x < s_across_x; x++) {
            const int canvas_dest_x = x + bdx,
                      canvas_dest_y = y + bdy,
                      sprite_src_x = x + axB,
                      sprite_src_y = y + ayB;

            const byte canvasPixel = m_pixels[(canvas_dest_y * width()) + canvas_dest_x],
                       spritePixel = bitmap.pixelAt(sprite_src_x, sprite_src_y),
                       mask = reduceToMask(spritePixel),
                       mixed = (spritePixel & mask) ^ (canvasPixel & ~mask);

            setPixel(canvas_dest_x, canvas_dest_y, mixed);
        }
    }
}

void Canvas::blitBitmapNonmasked(const Bitmap& bitmap, int destx, int desty)
{
    // figure out clipping
    const int canvW = width(),
              canvH = height(),
              sourceWidth = bitmap.width(), sourceHeight = bitmap.height(),
              topSpriteCanvasX = destx,
              topSpriteCanvasY = desty,
              endSpriteCanvasX = destx + sourceWidth,
              endSpriteCanvasY = desty + sourceHeight,
              topSpriteClippedX = (topSpriteCanvasX < 0) ? 0 : topSpriteCanvasX,
              topSpriteClippedY = (topSpriteCanvasY < 0) ? 0 : topSpriteCanvasY,
              endSpriteClippedX = std::min<int>(canvW, endSpriteCanvasX),
              endSpriteClippedY = std::min<int>(canvH, endSpriteCanvasY);

    const int bdx = std::max<int>(destx, 0), // beginning X offset of dest canvas
        bdy = std::max<int>(desty, 0), // beginning Y offset of dest canvas
        axB = topSpriteClippedX - topSpriteCanvasX, // beginning X offset of source bitmap
        ayB = topSpriteClippedY - topSpriteCanvasY, // beginning Y offset of source bitmap
        s_across_x = endSpriteClippedX - topSpriteClippedX, s_across_y = endSpriteClippedY - topSpriteClippedY;

    if (s_across_x <= 0) {
        return;
    }

    for (int y = 0; y < s_across_y; y++) {
        const int canvas_dest_y = y + bdy,
                  canvas_dest_x = bdx,
                  sprite_src_y = y + ayB,
                  sprite_src_x = axB;

        byte* const canvasRowBegin = &m_pixels[(canvas_dest_y * width()) + canvas_dest_x];
        const byte* const spriteRowBegin = bitmap.pixelPtrAt(sprite_src_x, sprite_src_y);

        memcpy(canvasRowBegin, spriteRowBegin, s_across_x);
    }
}
#endif // BITMAP_HPP_INCLUDED

void Canvas::drawRect(int x1, int y1, int x2, int y2, byte color)
{
    const int lx = std::min(x1, x2),
              ux = std::max(x1, x2),
              ly = std::min(y1, y2),
              uy = std::max(y1, y2),
              rectWidth = ux - lx,
              rectHeight = uy - ly;

    // figure out clipping
    const int canvW = width(),
              canvH = height(),
              topSpriteCanvasX = lx,
              topSpriteCanvasY = ly,
              endSpriteCanvasX = lx + rectWidth,
              endSpriteCanvasY = ly + rectHeight,
              topSpriteClippedX = (topSpriteCanvasX < 0) ? 0 : topSpriteCanvasX,
              topSpriteClippedY = (topSpriteCanvasY < 0) ? 0 : topSpriteCanvasY,
              endSpriteClippedX = std::min<int>(canvW, endSpriteCanvasX),
              endSpriteClippedY = std::min<int>(canvH, endSpriteCanvasY);

    const int bdx = std::max<int>(lx, 0), // beginning X offset of dest canvas
        bdy = std::max<int>(ly, 0), // beginning Y offset of dest canvas
        // axB = topSpriteClippedX - topSpriteCanvasX, // beginning X offset of source bitmap
        // ayB = topSpriteClippedY - topSpriteCanvasY, // beginning Y offset of source bitmap
        s_across_x = endSpriteClippedX - topSpriteClippedX, s_across_y = endSpriteClippedY - topSpriteClippedY;

    if (s_across_x <= 0) {
        return;
    }

    for (int y = 0; y < s_across_y; y++) {
        const int canvas_dest_y = y + bdy,
                  canvas_dest_x = bdx;
        byte* const canvasRowBegin = &m_pixels[(canvas_dest_y * width()) + canvas_dest_x];

        memset(canvasRowBegin, color, s_across_x);
    }
}

void Canvas::drawHorizLine(int x1, int y, int x2, byte color)
{
    TRIDBGMSG("  drawHLine(%d to %d on %d) : ", x1, x2, y);
    if ((y < 0) || (y >= height())) {
        TRIDBGMSG("FAIL\n");
        return;
    }

    const int lx = std::min(x1, x2),
              ux = std::max(x1, x2);

    if ((ux < 0) || (lx >= width())) {
        TRIDBGMSG("FAIL\n");
        return;
    }

    for (int xi = std::max(0, lx); xi <= std::min(ux, width() - 1); xi++)
        this->setPixel(xi, y, color);
    TRIDBGMSG("OK\n");
}

void Canvas::drawFlatTriangle(int x1, int y1, int x2, int y2, int x3, int y3, byte color)
{
    int topx = x1, topy = y1,
        midx = x2, midy = y2,
        btmx = x3, btmy = y3;

    if (topy > midy)
        swap2pair(topx, midx, topy, midy);
    if (topy > btmy)
        swap2pair(topx, btmx, topy, btmy);
    if (midy > btmy)
        swap2pair(midx, btmx, midy, btmy);

    TRIDBGMSG("--- tri (%d, %d)-(%d, %d)-(%d, %d) ---\n", topx, topy, midx, midy, btmx, btmy);

    const int x_mid_sub_top = midx - topx,
              x_btm_sub_top = btmx - topx,
              x_btm_sub_mid = btmx - midx,
              y_mid_sub_top = midy - topy,
              y_btm_sub_top = btmy - topy,
              y_btm_sub_mid = btmy - midy;

    if ((0 == y_btm_sub_top) || (btmy < 0) || (topy >= height())) {
        TRIDBGMSG("Tri FAIL(*)\n");
        return;
    }

    const float dlong = static_cast<float>(x_btm_sub_top) / static_cast<float>(y_btm_sub_top);

    float sx0 = static_cast<float>(topx),
          sx1 = sx0;

    if (0 == y_mid_sub_top)
        goto draw_lower;

    /* must enclose this below section in brackets, else clang whines about 'goto' above */
    {
        const float dupper = static_cast<float>(x_mid_sub_top) / static_cast<float>(y_mid_sub_top);
        TRIDBGMSG("UPPER:  dupper = %f\n", dupper);
        // draw upper sub-triangle
        for (int yi = topy; yi < std::min(midy, height()); yi++) {
            drawHorizLine(static_cast<int>(sx0), yi, static_cast<int>(sx1), color);
            sx0 += dupper;
            sx1 += dlong;
        }
    }

    // draw lower sub-triangle
draw_lower:
    if (0 == y_btm_sub_mid) {
        TRIDBGMSG("Tri OK(U)\n");
        return; // no need to draw lower sub-triangle
    }
    const float dlower = static_cast<float>(x_btm_sub_mid) / static_cast<float>(y_btm_sub_mid);
    TRIDBGMSG("LOWER:  dlower = %f\n", dlower);
    sx0 = static_cast<float>(midx);
    for (int yi = midy; yi < btmy; yi++) {
        drawHorizLine(static_cast<int>(sx0), yi, static_cast<int>(sx1), color);
        sx0 += dlower;
        sx1 += dlong;
    }

    TRIDBGMSG("Tri OK\n");
}

//! Find an indexed color closest to the specified arbitrary one, returning the index
std::pair<const color_t&, byte> Palette::findApproximateColor(color_t color) const
{
    int mindiff = std::numeric_limits<int>::max();
    int idx = 0;  // what ultimately gets returned

    for (byte i = 0; i < _colors.size(); i++)
    {
        const color_t& icolor = getColor(i);
        const int diff = std::abs(icolor.r - color.r) +
                         std::abs(icolor.g - color.g) +
                         std::abs(icolor.b - color.b);
        if (0 == diff)
            return { icolor, idx };

        if (diff < mindiff)
        {
            mindiff = diff;
            idx = i;
        }
    }

    return { getColor(idx), idx };
}
