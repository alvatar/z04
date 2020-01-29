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
           "emscripten_set_main_loop_arg")))
 (else #!void))

(c-define-type nk_context (struct "nk_context"))
(c-define-type nk_context* (pointer nk_context))
(c-define-type SDL_Event* (pointer (union "SDL_Event") #f))

(define gui-init
  (c-lambda ((pointer SDL_Window #f)) nk_context* "gui_init"))

(define gui-render
 (c-lambda () void "gui_render"))

(define gui-shutdown
 (c-lambda () void "nk_sdl_shutdown"))

;; (define gui-get-context
;;  (c-lambda () nk_context* "___return(_nk_ctx);"))

;; (define gui-input-begin
;;   (c-lambda (nk_context*) void "nk_input_begin"))

;; (define gui-input-end
;;  (c-lambda (nk_context*) void "nk_input_end"))

;; (define gui-sdl-handle-event
;;   (c-lambda (SDL_Event*) int "nk_sdl_handle_event"))

(c-define-type app_event (struct "app_event_t"))
(c-define-type app_event* (pointer app_event))

(define process-app-events
  (c-lambda () void "process_app_events"))

(define get-next-app-event
  (c-lambda () app_event* "get_next_app_event"))

(define app-event-type
  (c-lambda (app_event*) int "___return(___arg1->type);"))

(define app-event-data
  (c-lambda (app_event*) char-string "___return(___arg1->data);"))
