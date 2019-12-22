;;
;; Renderer
;;

(define example-scene-graph
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

;; TODO: Transform scene-graph -> scene-tree

(define example-scene-tree #f)

(define (renderer:init)
  (programs:init)
  (fonts:init)

  (set! example-scene-tree
        (list
         text:
         (make-text "Hello world!"
                    (make-box2d (make-vector2 -1.0 1.0) (make-vector2 1.0 -1.0))
                    '("assailand" 14)
                    (make-color 250 0 0 255)))))

(define (renderer:shutdown)
  (fonts:shutdown)
  (programs:shutdown))

(define (renderer:render)
  (glClearColor 0.0 0.0 0.0 1.0)
  (glClear GL_COLOR_BUFFER_BIT)

  ;; Create VBOs of dirty vertex objects
  (for-each
   (lambda (vo) (when (vertex-object-dirty vo)
             (vertex-object-vbo-set! vo (f32vector->gl-buffer (vertex-object-vertices vo) GL_STREAM_DRAW))
             (vertex-object-dirty-set! vo #f)))
   *vertex-objects*)

  ;; Render lines
  (with-gl-program
   (table-ref *programs* 'lines)
   (lambda (program-id)
     (for-each
      (lambda (vo) (gl-draw-with-vbo (vertex-object-vbo vo)
                                GL_LINE_STRIP
                                (f32vector-length (vertex-object-vertices vo))
                                (lambda ()
                                  (let ((pos (glGetAttribLocation program-id "position")))
                                    (glEnableVertexAttribArray pos)
                                    (glVertexAttribPointer pos 2 GL_FLOAT GL_FALSE 0 (integer->void* 0))))))
      *vertex-objects*)))

  ;; TEST XXX
  (with-gl-program
   (table-ref *programs* 'texture-2d)
   (lambda (program-id)
     (with-text-render-state
      (lambda () 'a)
      ;;(lambda () (text.render (cadr example-scene-tree)))
      )))

  ;; TODO: iterate  and render

  ;; Render texts
  ;; TODO
  ;; (with-program
  ;;  (table-ref *programs* 'texture-2d)
  ;;  (with-text-render-state
  ;;   ))
  )
