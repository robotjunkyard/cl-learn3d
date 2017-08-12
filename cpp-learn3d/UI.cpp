#include "UI.hpp"
#include "Bitmap.hpp"
#include "CanvasDef.hpp"

UI::UI(Canvas& canvas)
    : _targetCanvas(canvas)
{
    //ctor
}

UI::~UI()
{
    //dtor
}

Point UI::windowCoordinatesToCanvasCoordinates(Point winMousePos, int windowWidth, int windowHeight,
    int canvasWidth, int canvasHeight) const
{
    const float fww = windowWidth * 1.0f,
                fwh = windowHeight * 1.0f,
                wxext = winMousePos.first / fww,
                wyext = winMousePos.second / fwh;

    return Point(canvasWidth * wxext,
            canvasHeight * wyext);
}
