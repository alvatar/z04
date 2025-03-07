(c-declare #<<c-declare-end

#include <stdio.h>
#include <stdbool.h>

#include <SDL2/SDL.h>

#include <GLES2/gl2.h> // https://www.khronos.org/registry/OpenGL/api/GLES2/gl2.h

#define NK_INCLUDE_FIXED_TYPES
#define NK_INCLUDE_STANDARD_IO
#define NK_INCLUDE_STANDARD_VARARGS
#define NK_INCLUDE_DEFAULT_ALLOCATOR
#define NK_INCLUDE_VERTEX_BUFFER_OUTPUT
#define NK_INCLUDE_FONT_BAKING
#define NK_INCLUDE_DEFAULT_FONT
#define NK_IMPLEMENTATION
#define NK_SDL_GLES2_IMPLEMENTATION

#include "gui/nuklear.h"
#include "gui/nuklear_sdl_gles2.h"

struct nk_context *_nk_ctx;

#define MAX_VERTEX_MEMORY 512 * 1024
#define MAX_ELEMENT_MEMORY 128 * 1024

//
// App Events
//

#define EVENT_BUFFER_SIZE 10
#define EVENT_DATA_BUFFER_SIZE 256

#define ACTION_QUIT 1
#define ACTION_TEST 2
#define ACTION_TRANSLATE_SPACE 100
#define ACTION_SCALE_SPACE 101
#define ACTION_START_POLYLINE 1000

typedef struct app_event_t {
    int type;
    char *data;
} app_event;

static app_event event_list[EVENT_BUFFER_SIZE] = {};
int i_read_event = 0;
int i_write_event = 0;

void clear_app_events() {
    for (int i=0; i<i_write_event; i++) {
        char *buf = event_list[i].data;
        if (buf != NULL) {
            free(buf);
        }
    }
    i_read_event = 0;
    i_write_event = 0;
}

app_event* next_write_event() {
    app_event *e = NULL;
    if (i_write_event < EVENT_BUFFER_SIZE) {
        e = &event_list[i_write_event++];
        e->data = NULL;
    }
    return e;
}

app_event* next_read_event() {
    app_event *e = NULL;
    if (i_read_event < i_write_event) {
        e = &event_list[i_read_event++];
    }
    return e;
}

void process_app_events() {
    SDL_Event evt;
    app_event *aev;
    nk_input_begin(_nk_ctx);
    while (SDL_PollEvent(&evt)) {
        // Ignore SDL event if we don't have room for more
        switch (evt.type) {
        case SDL_QUIT:
            aev = next_write_event(); if (aev == NULL) continue;
            aev->type = ACTION_QUIT;
            break;
        case SDL_MOUSEMOTION:
        {
            const Uint8 *key_state = SDL_GetKeyboardState(NULL);
            if (key_state[SDL_SCANCODE_SPACE]) {
                aev = next_write_event(); if (aev == NULL) continue;
                aev->type = ACTION_TRANSLATE_SPACE;
                aev->data = malloc(EVENT_DATA_BUFFER_SIZE);
                snprintf(aev->data, EVENT_DATA_BUFFER_SIZE, "{\"x\": %d, \"y\": %d}", evt.motion.xrel, evt.motion.yrel);
            }
            break;
        }
        case SDL_MOUSEWHEEL:
        {
            const Uint8 *key_state = SDL_GetKeyboardState(NULL);
            if (key_state[SDL_SCANCODE_SPACE]) {
                aev = next_write_event(); if (aev == NULL) continue;
                aev->type = ACTION_SCALE_SPACE;
                aev->data = malloc(EVENT_DATA_BUFFER_SIZE);
                snprintf(aev->data, EVENT_DATA_BUFFER_SIZE, "{\"increment\": %d}", evt.wheel.y);
            }
            break;
        }
        }
        nk_sdl_handle_event(&evt);
    }
    nk_input_end(_nk_ctx);
}

bool js_push_event(int type, char *data) {
    printf("PUSHED EVENT %d %s\n", type, data);
    app_event *e = next_write_event();
    if (e == NULL) return false;
    e->type = type;
    e->data = data;
    return true;
}

//
// SDL App
//

#define SDL_Assert(x) do {if (!(x)) printf("Error: %s\n", SDL_GetError()); } while (0)

int _window_width, _window_height;
int _window_resolution_x, _window_resolution_y;
SDL_Window* _window;
SDL_GLContext _sdl_glctx;

void app_init()
{
   SDL_Assert(SDL_Init(SDL_INIT_VIDEO) == 0);

   // This is not necessary on OSX, but in some systems it is
   // SDL_SetHint(SDL_HINT_OPENGL_ES_DRIVER, "1");
   SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
   SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
   SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
   SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 2);
   SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_ES);

   SDL_DisplayMode d_mode;
   SDL_GetDesktopDisplayMode(0, &d_mode);
#ifdef __EMSCRIPTEN__
   int window_flags = SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN | SDL_WINDOW_RESIZABLE | SDL_WINDOW_ALLOW_HIGHDPI;
#else
   int window_flags = SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN | SDL_WINDOW_RESIZABLE;
#endif
   SDL_Window* w = SDL_CreateWindow("Ultra Awesome CAD",
                                    SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
                                    d_mode.w, d_mode.h,
                                    window_flags);

   SDL_Assert(w);
   _window = w;
   _window_width = d_mode.w;
   _window_height = d_mode.h;
   SDL_GL_GetDrawableSize(w, &_window_resolution_x, &_window_resolution_y);

   _sdl_glctx = SDL_GL_CreateContext(w);
   SDL_Assert(_sdl_glctx);

   SDL_Assert(SDL_GL_MakeCurrent(w, _sdl_glctx) == 0);
   SDL_GL_SetSwapInterval(0);

   /* PFNGLGETSTRINGPROC glGetString = SDL_GL_GetProcAddress("glGetString"); */
   /* PFNGLCLEARCOLORPROC glClearColor = SDL_GL_GetProcAddress("glClearColor"); */
   /* PFNGLCLEARPROC glClear = SDL_GL_GetProcAddress("glClear"); */
   printf("GL_VERSION = %s\n", glGetString(GL_VERSION));
   printf("GL_VENDOR = %s\n", glGetString(GL_VENDOR));
   printf("GL_RENDERER = %s\n", glGetString(GL_RENDERER));
   printf("Window size [w: %d, h:%d] resolution [w: %d, h: %d]\n", _window_width, _window_height, _window_resolution_x, _window_resolution_y);
}

void app_shutdown() {
    SDL_GL_DeleteContext(_sdl_glctx);
    SDL_DestroyWindow(_window);
    SDL_Quit();
}

//
// GUI
//

struct nk_context* gui_init() {
    _nk_ctx = nk_sdl_init(_window);

    /* Load Fonts: if none of these are loaded a default font will be used  */
    /* Load Cursor: if you uncomment cursor loading please hide the cursor */
    {
        struct nk_font_atlas *atlas;
        nk_sdl_font_stash_begin(&atlas);
        /*struct nk_font *droid = nk_font_atlas_add_from_file(atlas, "../../../extra_font/DroidSans.ttf", 14, 0);*/
        /*struct nk_font *roboto = nk_font_atlas_add_from_file(atlas, "../../../extra_font/Roboto-Regular.ttf", 16, 0);*/
        /*struct nk_font *future = nk_font_atlas_add_from_file(atlas, "../../../extra_font/kenvector_future_thin.ttf", 13, 0);*/
        /*struct nk_font *clean = nk_font_atlas_add_from_file(atlas, "../../../extra_font/ProggyClean.ttf", 12, 0);*/
        /*struct nk_font *tiny = nk_font_atlas_add_from_file(atlas, "../../../extra_font/ProggyTiny.ttf", 10, 0);*/
        /*struct nk_font *cousine = nk_font_atlas_add_from_file(atlas, "../../../extra_font/Cousine-Regular.ttf", 13, 0);*/
        nk_sdl_font_stash_end();
        /*nk_style_load_all_cursors(ctx, atlas->cursors);*/
        /*nk_style_set_font(ctx, &roboto->handle);*/
    }

    /* style.c */
    /*set_style(ctx, THEME_WHITE);*/
    /*set_style(ctx, THEME_RED);*/
    /*set_style(ctx, THEME_BLUE);*/
    /*set_style(ctx, THEME_DARK);*/
    return _nk_ctx;
}

void gui_render() {
struct nk_context *ctx = _nk_ctx;

if (nk_begin(ctx, "Demo", nk_rect(50, 50, 200, 200),
             NK_WINDOW_BORDER | NK_WINDOW_MOVABLE | NK_WINDOW_SCALABLE |
             NK_WINDOW_CLOSABLE | NK_WINDOW_MINIMIZABLE | NK_WINDOW_TITLE))
{
    nk_menubar_begin(ctx);
    nk_layout_row_begin(ctx, NK_STATIC, 25, 2);
    nk_layout_row_push(ctx, 45);
    if (nk_menu_begin_label(ctx, "FILE", NK_TEXT_LEFT, nk_vec2(120, 200)))
    {
        nk_layout_row_dynamic(ctx, 30, 1);
        nk_menu_item_label(ctx, "OPEN", NK_TEXT_LEFT);
        nk_menu_item_label(ctx, "CLOSE", NK_TEXT_LEFT);
        nk_menu_end(ctx);
    }
    nk_layout_row_push(ctx, 45);
    if (nk_menu_begin_label(ctx, "EDIT", NK_TEXT_LEFT, nk_vec2(120, 200)))
    {
        nk_layout_row_dynamic(ctx, 30, 1);
        nk_menu_item_label(ctx, "COPY", NK_TEXT_LEFT);
        nk_menu_item_label(ctx, "CUT", NK_TEXT_LEFT);
        nk_menu_item_label(ctx, "PASTE", NK_TEXT_LEFT);
        nk_menu_end(ctx);
    }
    nk_layout_row_end(ctx);
    nk_menubar_end(ctx);

    enum {EASY, HARD};
    static int op = EASY;
    static int property = 20;
    nk_layout_row_static(ctx, 30, 80, 1);
    if (nk_button_label(ctx, "button")) {
        app_event * aev = next_write_event();
        if (aev != NULL) aev->type = ACTION_START_POLYLINE;
    }

    nk_layout_row_dynamic(ctx, 30, 2);
    if (nk_option_label(ctx, "easy", op == EASY)) op = EASY;
    if (nk_option_label(ctx, "hard", op == HARD)) op = HARD;
    nk_layout_row_dynamic(ctx, 25, 1);
    nk_property_int(ctx, "Compression:", 0, &property, 100, 10, 1);
}
nk_end(ctx);

/* IMPORTANT: `nk_sdl_render` modifies some global OpenGL state
 * with blending, scissor, face culling, depth test and viewport and
 * defaults everything back into a default state.
 * Make sure to either a.) save and restore or b.) reset your own state after
 * rendering the UI. */
nk_sdl_render(NK_ANTI_ALIASING_ON, MAX_VERTEX_MEMORY, MAX_ELEMENT_MEMORY);
}

c-declare-end
)

(include "init-intf.scm")
