(import (scheme base))

(import (github.com/alvatar/sdl2))
(import (github.com/alvatar/ffi-utils))

(import (base))
(import (render))

(define (main)
  (when (< (SDL_Init SDL_INIT_VIDEO) 0) (SDL_LogCritical (string-append "Error initializing SDL " (SDL_GetError))))
  (SDL_Log "Initializing...\n")
  (let* ((mode* (alloc-SDL_DisplayMode))
         (window-width 800)
         (window-height 600)
         (window (SDL_CreateWindow "Ultra Awesome CAD"
                                   SDL_WINDOWPOS_UNDEFINED
                                   SDL_WINDOWPOS_UNDEFINED
                                   window-width
                                   window-height
                                   SDL_WINDOW_SHOWN)))
    (unless window (SDL_LogCritical (string-append "Error creating window: " (SDL_GetError))))

    (SDL_GL_SetAttribute SDL_GL_CONTEXT_MAJOR_VERSION 2)
    (SDL_GL_SetAttribute SDL_GL_CONTEXT_MINOR_VERSION 0)
    (SDL_GL_SetAttribute SDL_GL_DOUBLEBUFFER 1)
    (SDL_GL_SetAttribute SDL_GL_DEPTH_SIZE 24)

    (let ((glc (SDL_GL_CreateContext window)))
      (SDL_GL_SetSwapInterval 0)
      (SDL_CreateRenderer window -1 (bitwise-ior SDL_RENDERER_ACCELERATED SDL_RENDERER_TARGETTEXTURE))

      (renderer:init window-width window-height)

      (let/cc exit
              (let ((event* (alloc-SDL_Event)))
                (let loop ()
                  (let poll-events ()
                    (if (= 1 (SDL_PollEvent event*))
                        (when (= (SDL_Event-type event*) SDL_QUIT)
                          ;; TODO!
                          (exit)))
                    (renderer:render)
                    (SDL_GL_SwapWindow window)
                    (poll-events)))))

      (renderer:shutdown)

      (SDL_DestroyWindow window)
      (SDL_Quit)
      (SDL_Log "Exiting..."))))

(main)
