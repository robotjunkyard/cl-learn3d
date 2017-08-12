#include "canvas8.hpp"
#include "Bitmap.hpp"
#include "CanvasDef.hpp"
#include <array>

/* db32 palette by Richard "DawnBringer" Fhager
 * Link: http://pixeljoint.com/forum/forum_posts.asp?TID=16247
 */
const std::array<color_t, 256> raw_db32 = // comment to stop clang-format from putting first element here, goddamn it
    { { { 255, 0, 255, 0 }, //!< Sprite blitter requires color #0 of sprite shall be "transparent", but the 0 alpha *here* is not actually used anywhere directly (just for mnemonics)
        //!< Also, Magenta makes it easy to spot when a bug is going on -- if this ugly Magenta is seen, transparency isn't being honored somewhere. */
        { 0, 0, 0, 255 },
        { 34, 32, 52, 255 },
        { 69, 40, 60, 255 },
        { 102, 57, 49, 255 },
        { 143, 86, 59, 255 },
        { 223, 113, 38, 255 },
        { 217, 160, 102, 255 },
        { 238, 195, 154, 255 },
        { 251, 242, 54, 255 },
        { 153, 229, 80, 255 },
        { 106, 190, 48, 255 },
        { 55, 148, 110, 255 },
        { 75, 105, 47, 255 },
        { 82, 75, 36, 255 },
        { 50, 60, 57, 255 },
        { 63, 63, 116, 255 },
        { 48, 96, 130, 255 },
        { 91, 110, 225, 255 },
        { 99, 155, 255, 255 },
        { 95, 205, 228, 255 },
        { 203, 219, 252, 255 },
        { 255, 255, 255, 255 },
        { 155, 173, 183, 255 },
        { 132, 126, 135, 255 },
        { 105, 106, 106, 255 },
        { 89, 86, 82, 255 },
        { 118, 66, 138, 255 },
        { 172, 50, 50, 255 },
        { 217, 87, 99, 255 },
        { 215, 123, 186, 255 },
        { 143, 151, 74, 255 },
        { 138, 111, 48, 255 } } };

Palette make_db32_Palette()
{
    Palette pal(raw_db32);

    return pal;
}

