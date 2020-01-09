(c-declare #<<c-declare-end

#include <stdio.h>
#include <SDL2/SDL.h>
#include <GLES2/gl2.h> // https://www.khronos.org/registry/OpenGL/api/GLES2/gl2.h

#define Assert(x) do {if (!(x)) printf("Error: %s\n", SDL_GetError()); } while (0)

int window_width;
int window_height;
SDL_Window* window;
SDL_GLContext sdl_glctx;


void gl_init()
{
   Assert(SDL_Init(SDL_INIT_VIDEO) == 0);

   // This is not necessary on OSX, but in some systems it is
   // SDL_SetHint(SDL_HINT_OPENGL_ES_DRIVER, "1");
   SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
   SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
   SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
   SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 2);
   SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_ES);

   SDL_DisplayMode d_mode;
   SDL_GetDesktopDisplayMode(0, &d_mode);
   SDL_Window* w = SDL_CreateWindow("Ultra Awesome CAD",
                                    SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
                                    d_mode.w, d_mode.h,
                                    SDL_WINDOW_OPENGL|SDL_WINDOW_SHOWN|SDL_WINDOW_RESIZABLE|SDL_WINDOW_ALLOW_HIGHDPI);
   Assert(w);
   window = w;
   window_width = d_mode.w;
   window_height = d_mode.h;

   sdl_glctx = SDL_GL_CreateContext(w);
   Assert(sdl_glctx);

   Assert(SDL_GL_MakeCurrent(w, sdl_glctx) == 0);
   SDL_GL_SetSwapInterval(0);

   /* PFNGLGETSTRINGPROC glGetString = SDL_GL_GetProcAddress("glGetString"); */
   /* PFNGLCLEARCOLORPROC glClearColor = SDL_GL_GetProcAddress("glClearColor"); */
   /* PFNGLCLEARPROC glClear = SDL_GL_GetProcAddress("glClear"); */

   printf("GL_VERSION = %s\n",  glGetString(GL_VERSION));
   printf("GL_VENDOR = %s\n",  glGetString(GL_VENDOR));
   printf("GL_RENDERER = %s\n",  glGetString(GL_RENDERER));
}


c-declare-end
)

(c-define-type SDL_Window "SDL_Window")
(c-define-type SDL_GLContext "SDL_GLContext")

(define gl-init (c-lambda () void "gl_init"))
(define get-window-width (c-lambda () int "___return(window_width);"))
(define get-window-height (c-lambda () int "___return(window_height);"))
(define get-window (c-lambda () (pointer SDL_Window #f) "___return(window);"))
(define get-sdl-glcontext (c-lambda () SDL_GLContext "___return(sdl_glctx);"))

(cond-expand
 (emscripten
(c-declare #<<c-declare-end
#include "emscripten.h"
c-declare-end
)

(define emscripten_set_main_loop_arg
 (c-lambda ((nonnull-function ((pointer void #f)) void) (pointer void) int int)
           void
           "emscripten_set_main_loop_arg"))

(define emscripten_get_now (c-lambda () double "emscripten_get_now"))
)
 (else #!void))