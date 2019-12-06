(load "ffi/system/cintf")
;;(import (ffi/system/cintf))
(import (ffi/memory))
(import (github.com/alvatar/sdl2))


(define (main)
  (if (< (SDL_Init SDL_INIT_VIDEO) 0) (SDL_LogCritical (string-append "Error initializing SDL " (SDL_GetError))))
  (display "OK SDL_Init\n")
  (let* ((window-width 640)
         (window-height 480)
         (window (SDL_CreateWindow "Ultra Awesome CAD"
                                   SDL_WINDOWPOS_UNDEFINED
                                   SDL_WINDOWPOS_UNDEFINED
                                   window-width
                                   window-height
                                   SDL_WINDOW_SHOWN)))
    (if (not window) (SDL_LogCritical (string-append "Error creating window: " (SDL_GetError))))
    (let ((wmi (alloc-SDL_SysWMinfo)))
      (SDL_GetVersion (SDL_SysWMinfo-version wmi))
      (if (not (SDL_GetWindowWMInfo window wmi))
          (SDL_LogCritical (string-append "Error getting Window Manager info: " (SDL_GetError))))


      (ffi/system/cintf#bgfx-init (*->void* wmi) window-width window-height)

      (SDL_DestroyWindow window)
      (SDL_Quit)
      (SDL_Log "Exiting..."))))

(main)

