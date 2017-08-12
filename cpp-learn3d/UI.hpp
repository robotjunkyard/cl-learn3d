#pragma once

#include "canvas8.hpp"

class Canvas;

class UI
{
    public:
        UI(Canvas& canvas);
        virtual ~UI();

        Point windowCoordinatesToCanvasCoordinates (Point winMousePos,
            int windowWidth, int windowHeight,
            int canvasWidth = CANVAS_WIDTH, int canvasHeight = CANVAS_HEIGHT) const;

    private:
        Canvas& _targetCanvas;
};
