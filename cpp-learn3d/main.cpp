#ifdef _MSC_VER
#pragma warning(push, 0)
#endif
#include <SDL2\SDL.h>
#ifdef _MSC_VER
#pragma warning(pop)
#endif

#include "Bitmap.hpp"
#include "Camera.hpp"
#include "CanvasDef.hpp"
#include "Mat.hpp"
#include "Mesh.hpp"
#include "Quat.hpp"
#include "Render.hpp"
#include "UI.hpp"
#include "canvas8.hpp"
#include <cstdio>

bool paused = false;
const int framerateLock = 60;
const int screenTicksPerFrame = 1000 / framerateLock;

Palette make_db32_Palette();

#undef main // fuck off, SDL

int main()
{
    Camera camera(CANVAS_WIDTH, CANVAS_HEIGHT,
        Vec3(3.0f, 3.0f, 0.0f),
        Vec3(0.0f, 0.0f, 0.0f),
        Vec3(0.0f, 0.0f, 1.0f),
        90.0f,
        1.0f, // near
        80.0f); // far

    Mesh mesh = Mesh::loadMesh("models/unitcubes.obj");

    Palette db32 = make_db32_Palette();
    Canvas canvas(db32, CANVAS_WIDTH, CANVAS_HEIGHT);
    UI ui(canvas);

    SDL_Init(SDL_INIT_EVERYTHING);
    atexit(SDL_Quit);

    const int windowWidth = CANVAS_WIDTH * canvasToWindowScale,
              windowHeight = CANVAS_HEIGHT * canvasToWindowScale;

    SDL_Window* const window = SDL_CreateWindow("cpp-learn3d",
        SDL_WINDOWPOS_UNDEFINED,
        SDL_WINDOWPOS_UNDEFINED,
        windowWidth,
        windowHeight,
        SDL_WINDOW_SHOWN //| SDL_WINDOW_FULLSCREEN_DESKTOP
    );

    SDL_Renderer* const renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
    SDL_RendererInfo info;
    SDL_GetRendererInfo(renderer, &info);

    printf("Renderer name: %s\n", info.name);
    printf("Texture formats: \n");
    for (Uint32 i = 0; i < info.num_texture_formats; i++) {
        printf("    %s\n", SDL_GetPixelFormatName(info.texture_formats[i]));
    }

    SDL_Texture* const texture = SDL_CreateTexture(renderer,
        SDL_PIXELFORMAT_ARGB8888,
        SDL_TEXTUREACCESS_STREAMING,
        CANVAS_WIDTH,
        CANVAS_HEIGHT);

    SDL_Event event;
    bool running = true;
    unsigned int frame = 0;

    while (running) {
        unsigned int ticksBeforeRender = SDL_GetTicks();

        int mouse_x, mouse_y;
        SDL_GetMouseState(&mouse_x, &mouse_y);

        while (SDL_PollEvent(&event)) {
            if ((SDL_QUIT == event.type) || (SDL_KEYDOWN == event.type && SDL_SCANCODE_ESCAPE == event.key.keysym.scancode)) {
                running = false;
                break;
            }

            if ((SDL_KEYDOWN == event.type) && (SDL_SCANCODE_SPACE == event.key.keysym.scancode))
                paused = !paused;
        }

        // DO ALL THE DRAWING TO CANVAS HERE
        canvas.clear();
        Point canvasMousePos = ui.windowCoordinatesToCanvasCoordinates(Point(mouse_x, mouse_y),
            windowWidth,
            windowHeight,
            CANVAS_WIDTH,
            CANVAS_HEIGHT);

        int cx = canvasMousePos.first,
            cy = canvasMousePos.second;

        {
            // tests FOV
            /*static float fovdelta = 0.0f;
            fovdelta += 0.01f;
            float fov = 90 + 80 * sin(fovdelta);
            printf("fov = %f\n", fov);*/

            // trying to troubleshoot LookAt bullshit
            static float eyedelta = 0.0f;
            eyedelta += 0.01f;

            //float eyedist = 32 + 128.0 * sin(eyedelta);
            float eyex = 1 * sin(eyedelta),
                  eyey = 1 * cos(eyedelta),
                  eyez = 0.0; // 7 * sin(eyedelta);

            camera.lookAt(Vec3(eyex, eyey, eyez),
                Vec3(0.0, 0.0, 0.0),
                Vec3(0.0, 0.0, 1.0));
            camera.setPerspectiveProjection(90.0, 0.1, 10.0);
        }

        drawMesh(canvas, camera, mesh);

        canvas.updateSDLTexture(texture); // present 8bit AxB --into--> 32bit NxM
        SDL_RenderCopy(renderer, texture, NULL, NULL);
        SDL_RenderPresent(renderer);

        unsigned int ticksAfterRender = SDL_GetTicks();
        unsigned int tickDiff = ticksAfterRender - ticksBeforeRender;

        if (tickDiff < screenTicksPerFrame) {
            SDL_Delay(screenTicksPerFrame - tickDiff);
        }

        frame++;
    }

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();
}
