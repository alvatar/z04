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

(define *scene-graph* '())


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
  (glEnable GL_MULTISAMPLE_EXT)
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

(define *render-layers* #f)

(define (renderer:load-scene!)
  (let [(graph (get-test-data))]
    (set! *scene-graph* graph)
    (set! *render-layers* (graph->layers graph))))

(define (renderer:render)
  (let ((time-init (current-time)))
    (glClearColor 0.0 0.0 0.0 1.0)
    (glClear GL_COLOR_BUFFER_BIT)

    (render-layers.render *render-layers*)

    ;; Render debug texts
    (with-gl-program
     'texture-2d
     (lambda (program-id)
       (with-text-overlay-render-state
        program-id
        (lambda () (for-each text.render
                        (list (make-text (format "FPS: ~a" (/ 1 (- (time->seconds (current-time)) (time->seconds time-init))))
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
;; Fake data for testing
;;

(define (get-test-data)
  ;; TODO: load based on definitions form the scene graph
  (fonts:install "assailand" 14 "fonts/assailand/hinted-Assailand-Medium.ttf")
  (fonts:install "assailand" 25 "fonts/assailand/hinted-Assailand-Medium.ttf")
  (fonts:install "assailand" 34 "fonts/assailand/hinted-Assailand-Medium.ttf")

  (let [(obj-uuid (uuid-v4))]
    `(root:
      (objects:
       ((id: ,obj-uuid) (polyline: (points: (100.0 50.0) (500.0 400.0) (200.0 20.0) (33.0 100.0))))
       ((id: ,(uuid-v4)) (polyline: (points: (9 1) (0 0) (2 2) (3 3)))))
      (group:
       (id: ,(uuid-v4))
       (prop: (name: "my group")
              (tags: "main"))
       (elements:
        (polyline: (id: ,(uuid-v4)) (prop: (tags: "helpers"))
                   (points: (1 400) (0 0) (20 200) (300 800)))
        (polyline: (id: ,(uuid-v4))
                   (points: (3 200) (100 400) (500 20) (20 3000)))
        (polyline: (id: ,(uuid-v4))
                   (points: (-200 100) (400 400) (20 400) (0 30)))
        (ref: (id: ,(uuid-v4)) (object: ,obj-uuid))))
      (text:
       (id: ,(uuid-v4))
       (content: "Hello CAD!")
       (font: ("assailand" 34))
       (color: (255 255 255 255))
       (box2d: (100.0 100.0) (40.0 34.0)))
      (text:
       (id: ,(uuid-v4))
       (content: "Bye CAD!")
       (font: ("assailand" 34))
       (color: (255 255 255 255))
       (box2d: (10.0 10.0) (40.0 34.0)))
      (text:
       (id: ,(uuid-v4))
       (content: "Looking good...")
       (font: ("assailand" 34))
       (color: (255 255 255 255))
       (box2d: (500.0 10.0) (40.0 34.0))))))
