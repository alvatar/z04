;;
;; Vertex Buffer Objects
;;

(define-type vertex-object
  vertices
  dirty (vbo unprintable:))
(define *vertex-objects*
  (list
   (make-vertex-object (f32vector 0.0 0.5 0.5 -0.5 -0.5 -0.5) #t #f)
   (make-vertex-object (f32vector 1.0 1.0 1.5 -1.5 -1.5 -1.5) #t #f)
   (make-vertex-object (f32vector 2.0 -0.5 1.5 -1.5 -0.5 1.5) #t #f)))

;;! Creates a new OpenGL VBO from a given f32vector.
(define (f32vector->gl-buffer vertex-data-vector buffer-type)
  (let ((buffer-id* (alloc-GLuint* 1)))
    (glGenBuffers 1 buffer-id*)
    (glBindBuffer GL_ARRAY_BUFFER (*->GLuint buffer-id*))
    (glBufferData GL_ARRAY_BUFFER
                  (* (f32vector-length vertex-data-vector) GLfloat-size)
                  (f32vector->GLfloat* vertex-data-vector)
                  buffer-type)
    (glBindBuffer GL_ARRAY_BUFFER 0)
    buffer-id*))

;;! Draws the given vbo with a particular program. The callback is
;; used to set up the attributes of the dynamic attributes
(define (gl-draw-with-vbo vbo-id* type count attribs-callback)
  (let ((vbo-id (*->GLuint vbo-id*)))
    (when (check-gl-error (glBindBuffer GL_ARRAY_BUFFER vbo-id))
      (attribs-callback)
      (check-gl-error (glDrawArrays type 0 count))
      (glBindBuffer GL_ARRAY_BUFFER 0))))
