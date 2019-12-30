(import (scheme base))

(import (github.com/alvatar/base)
        (github.com/alvatar/base functional)
        (github.com/alvatar/sdl2)
        (github.com/alvatar/ffi-utils))

(import (core)
        (render))

;; TODO: extract in library
(define (vector-find f v)
  (let [(l (vector-length v))]
    (let loop ((idx 0))
      (and (< idx l)
           (if-let [val (f (vector-ref v idx))]
                   val
                   (loop (+ idx 1)))))))

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
      ;; TODO: load based on definitions form the scene graph
      (render-fonts:install "assailand" 14 "fonts/assailand/hinted-Assailand-Medium.ttf")
      (render-fonts:install "assailand" 25 "fonts/assailand/hinted-Assailand-Medium.ttf")
      (render-fonts:install "assailand" 34 "fonts/assailand/hinted-Assailand-Medium.ttf")
      (receive (render-tree render-layers)
          (renderer:load-scene! (core-graph:get-test-data))
        ;; (pp (vector-length (node-element render-tree)))
        (let [(group (vector-find (lambda (x) (and (eq? group: (node-type x)) x)) (node-element render-tree)))]
          ;;(pp group)
          (println "***********************************")
          ;; (pp (scene-tree.add-node render-tree (make-node "my-node" text: #f group 0)))
          (pp (scene-tree.remove-node render-tree group))
          ))
      ;;--------

      ;; (SDL_SetRelativeMouseMode SDL_TRUE)
      (let/cc exit
              (let ((event* (alloc-SDL_Event))
                    (mouse-down #f)
                    (key-down #f))
                (let loop ()
                  (if (= 1 (SDL_WaitEvent event*))
                      (cond ((= (SDL_Event-type event*) SDL_QUIT)
                             (exit))
                            ((= (SDL_Event-type event*) SDL_KEYDOWN)
                             (let [(key-event* (*->void* event*))]
                               (when (zero? (SDL_KeyboardEvent-repeat key-event*))
                                 (let [(key (-> key-event* SDL_KeyboardEvent-keysym SDL_Keysym-sym))]
                                   (cond ((= key SDLK_SPACE)
                                          (set! key-down 'space)))))))
                            ((= (SDL_Event-type event*) SDL_KEYUP)
                             (set! key-down #f))
                            ((= (SDL_Event-type event*) SDL_MOUSEBUTTONDOWN)
                             (set! mouse-down #t))
                            ((= (SDL_Event-type event*) SDL_MOUSEBUTTONUP)
                             (set! mouse-down #f))
                            ((= (SDL_Event-type event*) SDL_MOUSEMOTION)
                             (when mouse-down
                               (let [(mouse-event* (*->void* event*))]
                                 (renderer:translate-view! (SDL_MouseMotionEvent-xrel mouse-event*)
                                                           (SDL_MouseMotionEvent-yrel mouse-event*)))))
                            ((= (SDL_Event-type event*) SDL_MOUSEWHEEL)
                             (renderer:scale-view! (SDL_MouseWheelEvent-y (*->void* event*))))))
                  (renderer:render)
                  (SDL_GL_SwapWindow window)
                  (loop))))

      (renderer:shutdown)

      (SDL_DestroyWindow window)
      (SDL_Quit)
      (SDL_Log "Exiting..."))))

(main)
