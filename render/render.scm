;; Globals
(define *screen-width*)
(define *screen-height*)
(define *base-matrix*)
(define *gl-base-matrix*)

(define *translation-vector2* (make-vector2 0.0 0.0))
(define *scaling-factor* 1.0)
(define *perspective-matrix*)
(define *gl-perspective-matrix*)

(define *debug-texts*)

(define *render-layers* #f)
(define *render-tree* #f)



;;
;; Space and projection
;;

(define (perspective-matrix-update!)
  (set! *perspective-matrix*
        (matrix* (matrix* *base-matrix*
                          (make-scaling-matrix *scaling-factor* *scaling-factor* 1.0))
                 (make-translation-matrix (vector2-x *translation-vector2*) (vector2-y *translation-vector2*) 0.0)))
  (set! *gl-perspective-matrix*
        (matrix->GLfloat* (matrix.map exact->inexact *perspective-matrix*))))

;;
;; Render
;;

(define (renderer:update-view! screen-width screen-height)
  (set! *screen-width* screen-width)
  (set! *screen-height* screen-height)
  (set! *base-matrix*
        (matrix* (make-translation-matrix -1.0 1.0 0.0)
                 (make-scaling-matrix (/ 2.0 screen-width) (/ -2.0 screen-height) 1.0)))
  (set! *gl-base-matrix* (matrix->GLfloat* (matrix.map exact->inexact *base-matrix*)))
  (perspective-matrix-update!))

(define (renderer:init width height)
  ;; (check-gl-error (glEnable GL_MULTISAMPLE_EXT))
  (check-gl-error (glDepthFunc GL_NEVER))
  (check-gl-error (glDisable GL_DEPTH_TEST))
  (renderer:update-view! width height)
  (fonts:init)
  (programs:init))

(define (renderer:translate-view! x y)
  (update! *translation-vector2*
           (lambda (v) (vector2+ v (make-vector2 (/ x *scaling-factor*)
                                            (/ y *scaling-factor*)))))
  (perspective-matrix-update!))

(define (renderer:scale-view! s)
  (update! *scaling-factor* (lambda (f) (- f (/ s 100.0))))
  (perspective-matrix-update!))

(define (renderer:shutdown)
  (fonts:shutdown)
  (programs:shutdown))

(define (renderer:load-scene! graph)
  (set! *render-tree* (graph->scene-tree graph))
  (set! *render-layers* (scene-tree->scene-layers *render-tree*))
  (values *render-tree* *render-layers*))

(define (renderer:render)
  (let ((time-init (time->seconds (current-time))))
    (glClearColor 0.0 0.0 0.0 1.0)
    (glClear GL_COLOR_BUFFER_BIT)

    (render-layers.render *render-layers*)
    ;; (println "TIME native: " time-init (time->seconds (current-time)))

    ;; Render debug texts
    (with-gl-program
     'texture-2d
     (lambda (program-id)
       (with-text-overlay-render-state
        program-id
        (lambda () (for-each (text.render program-id)
                        (list (make-text (format "FPS: ~a" (- time-init (time->seconds (current-time))))
                                         (make-box2d (make-vector2 10.0 (- *screen-height* 55.0)) (make-vector2 100.0 100.0))
                                         '("assailand" 25)
                                         (make-color 255 255 255 255))
                              (make-text (format "Scale factor: ~a" *scaling-factor*)
                                         (make-box2d (make-vector2 10.0 (- *screen-height* 40.0)) (make-vector2 100.0 100.0))
                                         '("assailand" 25)
                                         (make-color 255 255 255 255))
                              (make-text (format "Translation factor: [~a, ~a]" (vector2-x *translation-vector2*) (vector2-y *translation-vector2*))
                                         (make-box2d (make-vector2 10.0 (- *screen-height* 25.0)) (make-vector2 100.0 100.0))
                                         '("assailand" 25)
                                         (make-color 255 255 255 255))))))))))

;;
;; Utils
;;

(define (SDL_Color->color sdl-color)
  (make-color
   (SDL_Color-r sdl-color)
   (SDL_Color-g sdl-color)
   (SDL_Color-b sdl-color)
   (SDL_Color-a sdl-color)))

(define-memoized (color->SDL_Color color)
  (let ((sdl-color* (alloc-SDL_Color)))
    (SDL_Color-r-set! sdl-color* (color-r color))
    (SDL_Color-g-set! sdl-color* (color-g color))
    (SDL_Color-b-set! sdl-color* (color-b color))
    (SDL_Color-a-set! sdl-color* (color-a color))
    (*->SDL_Color sdl-color*)))
