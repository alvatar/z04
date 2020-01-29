;;
;; SDL App
;;

(c-define-type SDL_Window "SDL_Window")
(c-define-type SDL_GLContext "SDL_GLContext")

(define app:init (c-lambda () void "app_init"))
(define app:shutdown (c-lambda () void "app_shutdown"))
(define get-window-width (c-lambda () int "___return(_window_width);"))
(define get-window-height (c-lambda () int "___return(_window_height);"))

;;
;; Emscripten
;;

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

;;
;; Nuklear GUI
;;

(c-define-type nk_context (struct "nk_context"))
(c-define-type nk_context* (pointer nk_context))
(c-define-type SDL_Event* (pointer (union "SDL_Event") #f))

(define gui:init
  (c-lambda () nk_context* "gui_init"))

(define gui:render
 (c-lambda () void "gui_render"))

(define gui:shutdown
 (c-lambda () void "nk_sdl_shutdown"))

;;
;; App Events
;;

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
