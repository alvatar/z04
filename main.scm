(import (gambit))

(import (base)
        (base functional)
        (gles2)
        (sdl2)
        (ffi-utils))

(import (core)
        (render))

(cond-expand
 (emscripten (include "init.scm")
             (c-define (c-main-loop args) ((pointer void)) void "c_main_loop" "static"
                       (let/cc exit (main-loop exit exit))))
 (else #!void))

(define (main-loop loop exit)
  (process-app-events)
  (while [ev (get-next-app-event)]
         ;; HERE all events are processed in Scheme
         (println "TYPE " (app-event-type ev))
         (println "DATA " (app-event-data ev))
         ;; (if ev
         ;;     (let [(listeners (hash-table-get event-listeners (event-type ev)))]
         ;;       (for-each (lambda (f) (f ev)) listeners))
         ;;     (exit))
         )

  (renderer:render)
  (gui-render)

  (SDL_GL_SwapWindow (get-window))
  (loop))

(define (main)
  (gl-init)
  (gui-init (get-window))
  (renderer:init (get-window-width) (get-window-height))
  ;;--------
  ;; TODO: load based on definitions form the scene graph
  (render-fonts:install "assailand" 14 "assets/fonts/assailand/hinted-Assailand-Medium.ttf")
  (render-fonts:install "assailand" 25 "assets/fonts/assailand/hinted-Assailand-Medium.ttf")
  (render-fonts:install "assailand" 34 "assets/fonts/assailand/hinted-Assailand-Medium.ttf")
  (receive (render-tree render-layers)
      (renderer:load-scene! (core-graph:get-test-data))
    ;; (pp (vector-length (node-element render-tree)))
    (println "Scene loaded.")
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

  (cond-expand
   (gles2
    (let/cc exit (let loop () (main-loop loop exit))))
   (emscripten
    (emscripten_set_main_loop_arg c-main-loop #f -1 1)))

  (renderer:shutdown)
  (gui-shutdown)

  (SDL_GL_DeleteContext (get-sdl-glcontext))
  (SDL_DestroyWindow (get-window))
  (SDL_Quit)
  (println "Exiting..."))

(main)



;; (define main-loop
;;   (let ((event* (alloc-SDL_Event))
;;         (mouse-down #f)
;;         (key-down #f))
;;     (lambda (loop exit)
;;       (gui-input-begin (gui-get-context))
;;       (let loop-event []
;;         (when (= 1 (SDL_PollEvent event*))
;;           (cond ((= (SDL_Event-type event*) SDL_QUIT)
;;                  (exit))
;;                 ((= (SDL_Event-type event*) SDL_KEYDOWN)
;;                  (let [(key-event* (*->void* event*))]
;;                    (when (zero? (SDL_KeyboardEvent-repeat key-event*))
;;                      (let [(key (-> key-event* SDL_KeyboardEvent-keysym SDL_Keysym-sym))]
;;                        (cond ((= key SDLK_SPACE)
;;                               (set! key-down 'space)))))))
;;                 ((= (SDL_Event-type event*) SDL_KEYUP)
;;                  (set! key-down #f))
;;                 ((= (SDL_Event-type event*) SDL_MOUSEBUTTONDOWN)
;;                  (set! mouse-down #t))
;;                 ((= (SDL_Event-type event*) SDL_MOUSEBUTTONUP)
;;                  (set! mouse-down #f))
;;                 ((= (SDL_Event-type event*) SDL_MOUSEMOTION)
;;                  (when mouse-down
;;                    (let [(mouse-event* (*->void* event*))]
;;                      (renderer:translate-view! (SDL_MouseMotionEvent-xrel mouse-event*)
;;                                                (SDL_MouseMotionEvent-yrel mouse-event*)))))
;;                 ((= (SDL_Event-type event*) SDL_MOUSEWHEEL)
;;                  (renderer:scale-view! (SDL_MouseWheelEvent-y (*->void* event*)))))

;;           (gui-sdl-handle-event event*)
;;           (renderer:render)
;;           (gui-render)

;;           (SDL_GL_SwapWindow (get-window))
;;           (loop-event)))
;;       (gui-input-end (gui-get-context))
;;       (loop))))
