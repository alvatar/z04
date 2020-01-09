;;
;; Vertex Buffer Objects
;; Manages local memory and graphic memory objects
;;

(define-type vertex-object
  constructor: make-vertex-object/internal
  vertices
  num-vertices
  vbo)

(define (make-vertex-object/empty)
  (make-vertex-object/internal #f 0 #f))

(define (make-vertex-object vertices num-vertices)
  (make-vertex-object/internal vertices num-vertices (f32vector->gl-buffer vertices GL_STREAM_DRAW)))

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

;;! refresh the internal data
(define (vertex-object.refresh!/internal vo)
  (when-let [vbo (vertex-object-vbo vo)] (glDeleteBuffers 1 vbo))
  (vertex-object-vbo-set!
   vo
   (f32vector->gl-buffer (vertex-object-vertices vo) GL_STREAM_DRAW)))

(define (vertex-object.update! vo vertices num-vertices)
  (vertex-object-vertices-set! vo vertices)
  (vertex-object-num-vertices-set! vo num-vertices)
  (vertex-object.refresh!/internal vo))

;;! Draws the given vbo with a particular program. The callback is
;; used to set up the attributes of the dynamic attributes
(define (vertex-object.render vo type pre-draw-cb #!optional post-draw-cb)
  (check-gl-error (glBindBuffer GL_ARRAY_BUFFER (-> vo vertex-object-vbo *->GLuint)))
  (pre-draw-cb)
  (check-gl-error (glDrawArrays type 0 (vertex-object-num-vertices vo)))
  (when post-draw-cb (post-draw-cb))
  (check-gl-error (glBindBuffer GL_ARRAY_BUFFER 0)))
