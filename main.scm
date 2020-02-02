(import (gambit))

(import (base)
        (base alist)
        (base functional)
        (gles2)
        (sdl2)
        (ffi-utils))

(import (core)
        (render))


(define (main-loop loop exit)
  (renderer:render)
  (gui:render)
  (swap-window)
  ;; We process events after rendering, since Nuklear will push the events during rendering
  (process-app-events)
  (while [ev (get-next-app-event)]
         (let [(action (app-event-type ev))
               (data (app-event-data ev))]
           (if (eqv? action action:quit)
               (exit)
               (for-each (lambda (f) (f data)) (get-action-listeners action)))))
  (clear-app-events)
  (loop))

(cond-expand
 (emscripten
  (include "init.scm")
  (c-define (c-main-loop args) ((pointer void)) void "c_main_loop" "static"
            (let/cc exit (main-loop exit exit)))
  (define (run-main-loop)
    (emscripten_set_main_loop_arg c-main-loop #f -1 1)))
 (else
  (define (run-main-loop)
    (let/cc exit (let loop () (main-loop loop exit))))))

(define (main)
  (app:init)
  (gui:init)
  (renderer:init (get-window-width) (get-window-height) (get-window-resolution-x) (get-window-resolution-y))

  (add-action-listener action:translate-space
                       (lambda (data) (renderer:translate-view! (get+ data "x") (get+ data "y"))))
  (add-action-listener action:scale-space
                       (lambda (data) (renderer:scale-view! (get+ data "increment"))))
  (add-action-listener action:start-polyline
                       (lambda (_) (println "POLYLINE BEGIN!")))

  ;;--------
  ;; TODO: load based on definitions form the scene graph
  (render-fonts:install "assailand" 14 "assets/fonts/assailand/hinted-Assailand-Medium.ttf")
  (render-fonts:install "assailand" 25 "assets/fonts/assailand/hinted-Assailand-Medium.ttf")
  (render-fonts:install "assailand" 34 "assets/fonts/assailand/hinted-Assailand-Medium.ttf")
  (receive (render-tree render-layers)
      (renderer:load-scene! (core-graph:get-test-data (get-window-width) (get-window-height)))
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

  (run-main-loop)

  (renderer:shutdown)
  (gui:shutdown)
  (app:shutdown)
  (println "Exiting..."))

(main)
