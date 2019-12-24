;;
;; Vertex Buffer Objects
;;

(define-type vertex-object
  vertices
  dirty? (vbo-data unprintable:))

;;! refresh the internal data if the dirty? flag is set
(define (vertex-object.refresh! vo)
  (when (vertex-object-dirty? vo)
    (vertex-object-vbo-data-set! vo (f32vector->gl-buffer (vertex-object-vertices vo) GL_STREAM_DRAW))
    (vertex-object-dirty?-set! vo #f)))

(define (vertex-object-vbo vo)
  (vertex-object.refresh! vo)
  (vertex-object-vbo-data vo))

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
