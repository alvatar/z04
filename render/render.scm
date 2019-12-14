(import (github.com/alvatar/ffi-utils))

(include "gl-util.scm")

(define vertex-shader-source
"
attribute vec4 position;
void main()
{
    gl_Position = vec4(position.xyz, 1.0);
}"
)

(define fragment-shader-source
"
void main()
{
    gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
}"
)

(define-structure vertex-object vertices dirty vbo)

(define vertex-objects
  (list
   (make-vertex-object (f32vector 0.0 0.5 0.5 -0.5 -0.5 -0.5) #t #f)
   (make-vertex-object (f32vector 1.0 1.0 1.5 -1.5 -1.5 -1.5) #t #f)
   (make-vertex-object (f32vector 2.0 -0.5 1.5 -1.5 -0.5 1.5) #t #f)))

;; TODO: memoize
(define (*gl-program*)
  (let ((vertex-shader (gl-create-shader GL_VERTEX_SHADER vertex-shader-source))
        (fragment-shader (gl-create-shader GL_FRAGMENT_SHADER fragment-shader-source)))
    (gl-create-program (list vertex-shader fragment-shader))))

(define (render)
  (let ((*program-id* (*gl-program*)))
    (glClearColor 0.0 0.0 0.0 1.0)
    (glClear GL_COLOR_BUFFER_BIT)

    ;; Create VBOs of dirty vertex objects
    (for-each
     (lambda (vo) (when (vertex-object-dirty vo)
               (vertex-object-vbo-set! vo (f32vector->gl-buffer (vertex-object-vertices vo) GL_STREAM_DRAW))
               (vertex-object-dirty-set! vo #f)))
     vertex-objects)

    (with-program
     *program-id*
     (lambda ()
       (for-each
        (lambda (vo) (gl-draw-with-vbo (vertex-object-vbo vo)
                                  GL_LINE_STRIP
                                  (f32vector-length (vertex-object-vertices vo))
                                  (lambda ()
                                    (let ((pos (glGetAttribLocation *program-id* "position")))
                                      (glEnableVertexAttribArray pos)
                                      (glVertexAttribPointer pos 2 GL_FLOAT GL_FALSE 0 (integer->void* 0))))))
        vertex-objects)))))
