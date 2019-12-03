#include "SDL2/SDL.h"
#include "SDL2/SDL_syswm.h"

#include <stdio.h>
#include <stdbool.h>

#include "bgfx/bgfx.h"
#include "bgfx/platform.h"

#include "cintf.h"


// int main(int argc, char* argv[]) {

//     SDL_Window *window;                    // Declare a pointer

//     if(SDL_Init( SDL_INIT_VIDEO ) < 0) {
//         printf("SDL could not initialize! SDL_Error: %s\n",
//                SDL_GetError());
//         exit(1);
//     }

//     // Create an application window with the following settings:
//     window = SDL_CreateWindow(
//         "z04",
//         SDL_WINDOWPOS_UNDEFINED,
//         SDL_WINDOWPOS_UNDEFINED,
//         640,
//         480,
//         SDL_WINDOW_SHOWN // | SDL_WINDOW_ALLOW_HIGHDPI | SDL_WINDOW_OPENGL
//     );

//     if (window == NULL) {
//         // In the case that the window could not be made...
//         printf("Could not create window: %s\n", SDL_GetError());
//         return 1;
//     }

//     SDL_SysWMinfo wmi;
//     SDL_VERSION(&wmi.version);
//     if (!SDL_GetWindowWMInfo(window, &wmi)) {
//         return 1;
//     }

int bgfx_init(void* window, int width, int height) {

    bgfx::PlatformData pd;
    //pd.nwh =wmi.info.cocoa.window;
    pd.nwh = window;
    bgfx::setPlatformData(pd);
    bgfx::renderFrame();

    //bgfxInit.platformData = pd
    //bgfxInit.type = BGFX_RENDERER_TYPE_COUNT; // Automatically choose a renderer.
    //bgfxInit.resolution.width = 640;
    //bgfxInit.resolution.height = 480;
    //bgfxInit.resolution.reset = BGFX_RESET_VSYNC;

    bgfx::init();

    bgfx::reset(width, height, BGFX_RESET_VSYNC);

    // Enable debug text.
    bgfx::setDebug(BGFX_DEBUG_TEXT /*| BGFX_DEBUG_STATS*/);

    // Set view rectangle for 0th view
    bgfx::setViewRect(0, 0, 0, uint16_t(width), uint16_t(height));

    // Clear the view rect
    bgfx::setViewClear(0, BGFX_CLEAR_COLOR | BGFX_CLEAR_DEPTH, 0x443355FF, 1.0f, 0);


    // Set empty primitive on screen
    bgfx::touch(0);

    // bgfx_platform_data_t pd;
    // and give the pointer to the window to pd
    // pd.nwh = wmi.info.cocoa.window;
    // pd.ndt          = NULL;
    // pd.context      = NULL;
    // pd.backBuffer   = NULL;
    // pd.backBufferDS = NULL;

    // // Tell bgfx about the platform and window
    // bgfx_set_platform_data(&pd);

    // printf("Initializing BGFX\n");

    // // Render an empty frame
    // bgfx_render_frame(-1);

    // bgfx_init_t bgfxInit;
    // bgfxInit.type = BGFX_RENDERER_TYPE_COUNT; // Automatically choose a renderer.
    // bgfxInit.resolution.width = 640;
    // bgfxInit.resolution.height = 480;
    // bgfxInit.resolution.reset = BGFX_RESET_VSYNC;

    // bgfx_init(&bgfxInit);


    // bgfx_set_view_clear(0, BGFX_CLEAR_COLOR | BGFX_CLEAR_DEPTH, 0x443355FF, 1.0f, 0);
    // bgfx_set_view_rect(0, 0, 0, 640, 480);
    return 0;
}

    // // Poll for events and wait till user closes window
    // bool quit = false;
    // SDL_Event currentEvent;
    // while(!quit) {
    //     while(SDL_PollEvent(&currentEvent) != 0) {
    //         if(currentEvent.type == SDL_QUIT) {
    //             quit = true;
    //         }
    //     }
    //     bgfx::frame();
    // }


    // // Close and destroy the window
    // SDL_DestroyWindow(window);

    // // Clean up
    // SDL_Quit();
    // return 0;

