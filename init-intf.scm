;;
;; SDL App
;;

(c-define-type SDL_Window "SDL_Window")
(c-define-type SDL_GLContext "SDL_GLContext")

(define app:init (c-lambda () void "app_init"))
(define app:shutdown (c-lambda () void "app_shutdown"))
(define get-window-width (c-lambda () int "___return(_window_width);"))
(define get-window-height (c-lambda () int "___return(_window_height);"))

(define swap-window
  (c-lambda () void "SDL_GL_SwapWindow(_window);"))

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
              "emscripten_set_main_loop_arg"))

;;;----------------------------------------------------------------------------

  (define ##jseval
    (c-lambda (char-string) char-string "emscripten_run_script_string"))

  (define jseval ##jseval)

  (define (##local-storage-get item)
    (##jseval
     (##string-append "localStorageGet(" (##object->string item) ");")))

  (define (##local-storage-set item val)
    (##jseval
     (##string-append "localStorageSet(" (##object->string item) ","
                      (##object->string val) ");")))

;;;----------------------------------------------------------------------------

  (define (##show-definition-of subject)
    (let ((s
           (cond ((##procedure? subject)
                  (##object->string (##procedure-name subject)))
                 (else
                  (##object->string subject)))))
      (##jseval
       (##string-append "open(\"http://www-labs.iro.umontreal.ca/~gambit/doc/gambit-c.html#"
                        (##escape-link (##string-append "Definition of " s))
                        "\")")))
    (##void))

  (set! ##help-hook ##show-definition-of)

  (##c-declare #<<end-of-c-declare

void wget_file_onload(const char *file) {
   // printf("wget_file_onload file=%s\n", file);
}

void wget_file_onerror(const char *file) {
   // printf("wget_file_onerror file=%s\n", file);
}

void wget_file(const char *url, const char *file) {
  emscripten_async_wget(url, file, wget_file_onload, wget_file_onerror);
}

end-of-c-declare
)

  (define ##wget-file
    (c-lambda (char-string char-string) void "wget_file")))

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

(define clear-app-events
  (c-lambda () void "clear_app_events"))

(define get-next-app-event
  (c-lambda () app_event* "next_read_event"))

(define app-event-type
  (c-lambda (app_event*) int "___return(___arg1->type);"))

(define app-event-data
  (c-lambda (app_event*) char-string "___return(___arg1->data);"))
