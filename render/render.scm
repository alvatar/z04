(define (perspective-matrix-update!)
  (set! *perspective-matrix* (matrix* (matrix* *base-matrix*
                                               (make-scaling-matrix *scaling-factor* *scaling-factor* 1.0))
                                      (make-translation-matrix (vector2-x *translation-vector2*) (vector2-y *translation-vector2*) 0.0)))
  (set! *gl-perspective-matrix* (matrix->GLfloat* (matrix.map exact->inexact *perspective-matrix*))))

(define (renderer:update-view! screen-width screen-height)
  (set! *screen-width* screen-width)
  (set! *screen-height* screen-height)
  (set! *base-matrix* (matrix* (make-translation-matrix -1.0 1.0 0.0)
                               (make-scaling-matrix (/ 2.0 screen-width) (/ -2.0 screen-height) 1.0)))
  (set! *gl-base-matrix* (matrix->GLfloat* (matrix.map exact->inexact *base-matrix*)))
  (perspective-matrix-update!))

(define (renderer:init width height)
  (glEnable GL_MULTISAMPLE_EXT)
  (renderer:update-view! width height)
  (fonts:init)
  (programs:init))

(define (renderer:translate-view! x y)
  (update! *translation-vector2* (lambda (v) (vector2+ v (make-vector2 (/ x *scaling-factor*)
                                                                  (/ y *scaling-factor*)))))
  (perspective-matrix-update!))

(define (renderer:scale-view! s)
  (update! *scaling-factor* (lambda (f) (- f (/ s 100.0))))
  (perspective-matrix-update!))

(define (renderer:shutdown)
  (fonts:shutdown)
  (programs:shutdown))

(define (renderer:render)
  (glClearColor 0.0 0.0 0.0 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  ;; Create VBOs of dirty vertex objects
  (for-each
   (lambda (vo) (when (vertex-object-dirty? vo) (vertex-object.refresh! vo)))
   *vertex-objects*)

  ;; Render lines
  (with-gl-program
   'lines
   (lambda (program-id)
     (for-each
      (lambda (vo) (gl-draw-with-vbo (vertex-object-vbo vo)
                                GL_LINE_STRIP
                                (f32vector-length (vertex-object-vertices vo))
                                (lambda ()
                                  (check-gl-error
                                   (glUniformMatrix4fv (glGetUniformLocation program-id "perspectiveMatrix") 1 GL_FALSE *gl-perspective-matrix*))
                                  (let ((pos (glGetAttribLocation program-id "position")))
                                    (glEnableVertexAttribArray pos)
                                    (glVertexAttribPointer pos 2 GL_FLOAT GL_FALSE 0 #f)))))
      *vertex-objects*)))

  ;; Render texts
  (with-gl-program
   'texture-2d
   (lambda (program-id)
     (with-text-render-state program-id
                             (lambda ()
                               (for-each
                                (lambda (e) (text.render (cadr e)))
                                *scene-tree*)))))

  ;; Render debug texts
  (with-gl-program
   'texture-2d
   (lambda (program-id)
     (with-overlay-text-render-state
      program-id
      (lambda () (for-each text.render
                      (list (make-text (format "Scale factor: ~a" *scaling-factor*)
                                       (make-box2d (make-vector2 10.0 (- *screen-height* 40.0)) (make-vector2 100.0 100.0))
                                       '("assailand" 14)
                                       (make-color 255 255 255 255))
                            (make-text (format "Translation factor: [~a, ~a]" (vector2-x *translation-vector2*) (vector2-y *translation-vector2*))
                                       (make-box2d (make-vector2 10.0 (- *screen-height* 25.0)) (make-vector2 100.0 100.0))
                                       '("assailand" 14)
                                       (make-color 255 255 255 255))))))))

  ;; TODO: iterate graph and render
  )


;; --------------------------
;; Fake data for testing
(define (renderer:set-test-data!)
  (fonts:install "assailand" 14 "fonts/assailand/hinted-Assailand-Medium.ttf")
  (fonts:install "assailand" 34 "fonts/assailand/hinted-Assailand-Medium.ttf")

  (set! *vertex-objects*
        (list
         (make-vertex-object
          (f32vector
           0. 0.
           (exact->inexact (/ *screen-width* 2)) (exact->inexact (/ *screen-height* 2)))
          #t #f)
         (make-vertex-object
          (f32vector
           100. 100.
           300. 0.)
          #t #f)))
  (set! *scene-graph*
        '(root:graph
          (group:
           (info: (name: "my group"))
           (elements:
            (polyline: (1 1) (0 0) (2 2) (3 3))
            (polyline: (3 2) (1 4) (5 2) (2 3))
            (polyline: (2 1) (4 4) (2 4) (0 3))))
          (text:
           (font: "assailand")
           (size: 14)
           (content: "Hello world!")
           (position: 0 0))))
  (set! *scene-tree*
        `((text:
           ,(make-text "Hello world!!"
                       (make-box2d (make-vector2 100.0 100.0) (make-vector2 40.0 34.0))
                       '("assailand" 34)
                       (make-color 255 255 255 255)))
          (text:
           ,(make-text "Yes Yes"
                       (make-box2d (make-vector2 300.0 100.0) (make-vector2 20.0 34.0))
                       '("assailand" 34)
                       (make-color 255 255 255 255))))))
