;;
;; Polyline
;;

(define-type polyline
  id: polyline-type
  constructor: make-polyline/internal
  points (num-points unprintable:)
  style
  dirty? vbo)

(define (make-polyline points style)
  (let [(pl (make-polyline/internal
             points
             (length points)
             style
             #t
             (make-vertex-object/empty)))]
    (polyline.refresh-vbo! pl)
    pl))

(define (polyline.update! pl #!key points style)
  (when points (polyline-points-set! pl points))
  (when style (polyline-style-set! pl style))
  (polyline-dirty?-set! pl #t))

(define (polyline.refresh-vbo! pl)
  (when (polyline-dirty?-set! pl #f)
    (let [(vertices (make-f32vector (* (polyline-num-points pl) 2)))
          (n 0)]
      (for-each (lambda (p)
                  (f32vector-set! vertices n (exact->inexact (vector2-x p)))
                  (f32vector-set! vertices (fx+ n 1) (exact->inexact (vector2-y p)))
                  (set! n (fx+ n 2)))
                (polyline-points pl))
      (vertex-object.update! (polyline-vbo pl) vertices) ; vbo
      (polyline-dirty?-set! pl #f))))

(define (with-polyline-render-state program-id f)
  (check-gl-error
   (glUniformMatrix4fv (glGetUniformLocation program-id "perspectiveMatrix") 1 GL_FALSE *gl-perspective-matrix*))
  (f))

(define (polyline.render pl)
  (vertex-object.render
   (polyline-vbo pl)
   GL_LINE_STRIP
   (lambda ()
     ;; (glGetAttribLocation program-id "position")
     (let ((pos 0))
       (glEnableVertexAttribArray pos)
       (glVertexAttribPointer pos 2 GL_FLOAT GL_FALSE 0 #f)))))
