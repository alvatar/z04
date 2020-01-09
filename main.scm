(import (gambit))

(import (base)
        (base functional)
        (gles2)
        (sdl2)
        (ffi-utils))

(import (core)
        (render))

(define main-loop
  (let ((event* (alloc-SDL_Event))
        (mouse-down #f)
        (key-down #f))
    (lambda (loop exit)
      (when (= 1 (SDL_PollEvent event*))
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
               (renderer:scale-view! (SDL_MouseWheelEvent-y (*->void* event*)))))
        (renderer:render)
        (SDL_GL_SwapWindow (get-window)))
      (loop))))

(define (alt-loop loop)
  (glClearColor (random-real) (random-real) 0.0 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (SDL_GL_SwapWindow (get-window))
  (loop))

(cond-expand
 (gles2
  (define (main)
    (gl-init)
    (renderer:init (get-window-width) (get-window-height))
    ;;--------
    ;; TODO: load based on definitions form the scene graph
    (render-fonts:install "assailand" 14 "assets/fonts/assailand/hinted-Assailand-Medium.ttf")
    (render-fonts:install "assailand" 25 "assets/fonts/assailand/hinted-Assailand-Medium.ttf")
    (render-fonts:install "assailand" 34 "assets/fonts/assailand/hinted-Assailand-Medium.ttf")
    (receive (render-tree render-layers)
        (renderer:load-scene! (core-graph:get-test-data))
      ;; (pp (vector-length (node-element render-tree)))
      (println "Scene loaded")
      ;;(pp render-tree)
      ;; (let [(group (scene-tree.find-child (lambda (n) (eq? (node-type n) group:)) render-tree))]
      ;;   ;;(pp group)
      ;;   (println "***********************************")
      ;;   ;; (pp (scene-tree.add-node render-tree (make-node "my-node" text: #f group 0)))
      ;;   (scene-tree.remove-node! group)
      ;;   (pp render-tree))
      )
    ;;--------

    ;; (SDL_SetRelativeMouseMode SDL_TRUE)
    (let/cc exit (let loop () (main-loop loop exit)))

    (renderer:shutdown)

    (SDL_GL_DeleteContext (get-sdl-glcontext))
    (SDL_DestroyWindow (get-window))
    (SDL_Quit)
    (println "Exiting...")))

 (emscripten
  (include "init.scm")
  (c-define (c-main-loop args) ((pointer void)) void "c_main_loop" "static"
            (let/cc exit (main-loop exit exit)))
  (define (main)
    (gl-init)
    (renderer:init (get-window-width) (get-window-height))
    (render-fonts:install "assailand" 14 "assets/fonts/assailand/hinted-Assailand-Medium.ttf")
    (render-fonts:install "assailand" 25 "assets/fonts/assailand/hinted-Assailand-Medium.ttf")
    (render-fonts:install "assailand" 34 "assets/fonts/assailand/hinted-Assailand-Medium.ttf")
    (receive (render-tree render-layers)
        (renderer:load-scene! (core-graph:get-test-data))
      (println "Scene loaded."))

    (emscripten_set_main_loop_arg c-main-loop #f -1 1)

    (renderer:shutdown)

    (SDL_GL_DeleteContext (get-sdl-glcontext))
    (SDL_DestroyWindow (get-window))
    (SDL_Quit)
    (println "Exiting..."))))

(main)
