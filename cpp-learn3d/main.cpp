#ifdef _MSC_VER
#pragma warning(push, 0)
#endif
#include <SDL2/SDL.h>
#ifdef _MSC_VER
#pragma warning(pop)
#endif

/*  #include "Bitmap.hpp"  */
#include "Camera.hpp"
#include "CanvasDef.hpp"
#include "Mat.hpp"
#include "Mesh.hpp"
#include "PredicateQuicksort.hpp"
#include "Quat.hpp"
#include "Render.hpp"
#include "UI.hpp"
#include "canvas8.hpp"
#include "Triangle.hpp"
#include <cstdio>
#include <vector>

bool paused = false;
const int framerateLock = 60;
const int screenTicksPerFrame = 1000 / framerateLock;

Palette make_db32_Palette();

#undef main // fuck off, SDL

int vtest(int i)
{
    return i;
}

int main(int argc, char* argv[])
{
    const Bitmap cursorpic(16, 16, "cursor.data");
    Camera camera(CANVAS_WIDTH, CANVAS_HEIGHT,
        Vec3(3.0f, 3.0f, 0.0f),
        Vec3(0.0f, 0.0f, 0.0f),
        Vec3(0.0f, 0.0f, 1.0f),
        90.0f,
        1.0f, // near
        80.0f); // far

    Triangle2 testTri { {16, 64}, {CANVAS_WIDTH/2,CANVAS_HEIGHT - 64}, {384, 8} };

    std::string meshname = "";
    if (argc == 2)
        meshname = argv[1];
    if (meshname == "")
        meshname = "obelisk";

    const Mesh mesh = Mesh::loadMesh(meshname);
    const auto* const meshtexture = mesh.getTexture();

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
        const unsigned int ticksBeforeRender = SDL_GetTicks();

        int mouse_x, mouse_y;
        SDL_PumpEvents();
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

        /* const Point canvasMousePos = ui.windowCoordinatesToCanvasCoordinates(Point(mouse_x, mouse_y),
            windowWidth,
            windowHeight,
            CANVAS_WIDTH,
            CANVAS_HEIGHT);
        const int cx = canvasMousePos.first,
                  cy = canvasMousePos.second; */

        {
            static float eyedelta = 0.0f;
            eyedelta += 0.01f;

            constexpr float h = 0.0f;
            const float eyex = 24 * sin(eyedelta),
                        eyey = 24 * cos(eyedelta),
                        eyez = 16.0f + h;

            camera.lookAt(Vec3(eyex, eyey, eyez),
                Vec3(0.0f, 0.0f, h),
                Vec3(0.0f, 0.0f, 1.0f));
            camera.setPerspectiveProjection(27.5f, 0.1f, 10.0f);
        }

        // determine mouse cursor position in canvas
        const Vec2 mcurpos = { mouse_x / canvasToWindowScale, mouse_y / canvasToWindowScale };

        // determine barycentric coords of debug triangle
        const Vec3 bary = testTri.barycentricCoordinates(mcurpos);

        canvas.blitBitmap(*mesh.getTexture(), 4, 16);
        canvas.drawFlatTriangle(testTri, bary.allGTE(0.0f) ? 12 : 4);
        //Render::drawMeshFlat(canvas, camera, mesh);
        Render::drawMeshTextured(canvas, camera, mesh);

        canvas.blitBitmap(cursorpic, mcurpos.x, mcurpos.y);
        printf("%f, %f, %f\n", bary.x, bary.y, bary.z);

        // present to user
        canvas.updateSDLTexture(texture); // present 8bit AxB --into--> 32bit NxM
        SDL_RenderCopy(renderer, texture, NULL, NULL);
        SDL_RenderPresent(renderer);

        const unsigned int ticksAfterRender = SDL_GetTicks();
        const unsigned int tickDiff = ticksAfterRender - ticksBeforeRender;

        if (tickDiff < screenTicksPerFrame) {
            SDL_Delay(screenTicksPerFrame - tickDiff);
        }

        frame++;
    }

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();
}
