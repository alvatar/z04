(import (scheme base))

(import (github.com/alvatar/sdl2))
(import (github.com/alvatar/ffi-utils))

(import (base))
(import (render))


(define (main)
  (when (< (SDL_Init SDL_INIT_VIDEO) 0) (SDL_LogCritical (string-append "Error initializing SDL " (SDL_GetError))))

  (SDL_Log "Initializing...\n")
  (SDL_GL_SetAttribute SDL_GL_MULTISAMPLEBUFFERS 1)
  (SDL_GL_SetAttribute SDL_GL_MULTISAMPLESAMPLES 16)
  (SDL_GL_SetAttribute SDL_GL_ALPHA_SIZE 8)
  (SDL_GL_SetAttribute SDL_GL_RED_SIZE 8)
  (SDL_GL_SetAttribute SDL_GL_GREEN_SIZE 8)
  (SDL_GL_SetAttribute SDL_GL_BLUE_SIZE 8)
  (SDL_GL_SetAttribute SDL_GL_DOUBLEBUFFER 1)
  (SDL_GL_SetAttribute SDL_GL_DEPTH_SIZE 0)
  (SDL_GL_SetAttribute SDL_GL_RETAINED_BACKING 1)
  (SDL_GL_SetAttribute SDL_GL_CONTEXT_MAJOR_VERSION 2)
  (SDL_GL_SetAttribute SDL_GL_CONTEXT_MINOR_VERSION 0)
  (SDL_GL_SetAttribute SDL_GL_DOUBLEBUFFER 1)
  (SDL_GL_SetAttribute SDL_GL_DEPTH_SIZE 24)

  (let* ((mode* (alloc-SDL_DisplayMode))
         (_ (SDL_GetDesktopDisplayMode 0 mode*))
         (window-width (SDL_DisplayMode-w mode*))
         (window-height (- (SDL_DisplayMode-h mode*) 50))
         (window (SDL_CreateWindow "Ultra Awesome CAD"
                                   SDL_WINDOWPOS_UNDEFINED
                                   SDL_WINDOWPOS_UNDEFINED
                                   window-width
                                   window-height
                                   SDL_WINDOW_SHOWN)))
    (unless window (SDL_LogCritical (string-append "Error creating window: " (SDL_GetError))))
    (let ((glc (SDL_GL_CreateContext window)))
      (SDL_GL_SetSwapInterval 0)
      (SDL_CreateRenderer window -1 (bitwise-ior SDL_RENDERER_ACCELERATED SDL_RENDERER_TARGETTEXTURE))

      (renderer:init window-width window-height)

      ;;--------
      (renderer:set-test-data!)
      ;;--------
      ;; (SDL_SetRelativeMouseMode SDL_TRUE)

      (let/cc exit
              (let ((event* (*->void* (alloc-SDL_Event)))
                    (mouse-down #f))
                (let poll-events ()
                  (if (= 1 (SDL_WaitEvent event*))
                      (cond ((= (SDL_Event-type event*) SDL_QUIT)
                             (exit))
                            ((= (SDL_Event-type event*) SDL_MOUSEBUTTONDOWN)
                             (set! mouse-down #t))
                            ((= (SDL_Event-type event*) SDL_MOUSEBUTTONUP)
                             (set! mouse-down #f))
                            ((= (SDL_Event-type event*) SDL_MOUSEMOTION)
                             (when mouse-down
                               (let ((x (SDL_MouseMotionEvent-xrel event*))
                                     (y (SDL_MouseMotionEvent-yrel event*)))
                                 (renderer:translate-view! x y))))
                            ((= (SDL_Event-type event*) SDL_MOUSEWHEEL)
                             (let ((s (SDL_MouseWheelEvent-y event*)))
                               (renderer:scale-view! s)))))
                  (renderer:render)
                  (SDL_GL_SwapWindow window)
                  (poll-events))))

      (renderer:shutdown)

      (SDL_DestroyWindow window)
      (SDL_Quit)
      (SDL_Log "Exiting..."))))

(main)
