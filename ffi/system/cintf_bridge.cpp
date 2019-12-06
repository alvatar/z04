#include "SDL2/SDL.h"
#include "SDL2/SDL_syswm.h"

#include <stdio.h>
#include <stdbool.h>

#include "bgfx/bgfx.h"
#include "bgfx/platform.h"

#include "cintf.h"


//     SDL_Window *window;
//     if(SDL_Init( SDL_INIT_VIDEO ) < 0) {
//         printf("SDL could not initialize! SDL_Error: %s\n",
//                SDL_GetError());
//         exit(1);
//     }
//     window = SDL_CreateWindow(
//         "z04",
//         SDL_WINDOWPOS_UNDEFINED,
//         SDL_WINDOWPOS_UNDEFINED,
//         640,
//         480,
//         SDL_WINDOW_SHOWN // | SDL_WINDOW_ALLOW_HIGHDPI | SDL_WINDOW_OPENGL
//     );
//     if (window == NULL) {
//         printf("Could not create window: %s\n", SDL_GetError());
//         return 1;
//     }
//     SDL_SysWMinfo wmi;
//     SDL_VERSION(&wmi.version);
//     if (!SDL_GetWindowWMInfo(window, &wmi)) {
//         return 1;
//     }

int bgfx_init(SDL_SysWMinfo* wmi, int width, int height) {
    bgfx::PlatformData pd;
    pd.nwh =wmi->info.cocoa.window;
    bgfx::setPlatformData(pd);

    bgfx::renderFrame();

    bgfx::init();

    bgfx::reset(width, height, BGFX_RESET_VSYNC);

    bgfx::setDebug(BGFX_DEBUG_TEXT /*| BGFX_DEBUG_STATS*/);

    bgfx::setViewRect(0, 0, 0, uint16_t(width), uint16_t(height));

    bgfx::setViewClear(0, BGFX_CLEAR_COLOR | BGFX_CLEAR_DEPTH, 0x443355FF, 1.0f, 0);

    bgfx::touch(0);

    bgfx::setViewClear(0, BGFX_CLEAR_COLOR | BGFX_CLEAR_DEPTH, 0x443355FF, 1.0f, 0);
    bgfx::setViewRect(0, 0, 0, 640, 480);

    // Poll for events and wait till user closes window
    bool quit = false;
    SDL_Event currentEvent;
    while(!quit) {
        while(SDL_PollEvent(&currentEvent) != 0) {
            if(currentEvent.type == SDL_QUIT) {
                quit = true;
            }
        }
        bgfx::frame();
    }

    return 0;
}
// SDL_DestroyWindow(window);
// SDL_Quit();


