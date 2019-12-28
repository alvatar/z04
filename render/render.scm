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

;;
;; Scenegraph
;;

(define-type node
  id: graph-node
  constructor: make-node/internal
  id type element dirty parent)

(define (make-node id type element parent)
  (make-node/internal id type element #f parent))

(define (scene-data->scene-tree graph)
  (define objects (make-table))
  (define (get-id-or-create ls) (if-let [idl (assq id: ls)] (cadr idl) (uuid-v4)))
  (when-let
   [root (memq root: graph)]
   (let recur ((parent root:)
               (graph (cdr root)))
     (if (null? graph)
         '()
         (match (car graph)
                ((,type . ,data)
                 (case type
                   ((objects:)
                    (for-each (lambda (obj) (table-set! objects (get obj id:) (recur parent (cdr obj)))) data)
                    (recur parent (cdr graph)))
                   ((group:)
                    (cons (if-let [elements (get+ data elements:)]
                                  (let [(node (make-node (get-id-or-create data) group: #f parent))]
                                    (node-element-set! node (recur node elements))
                                    node)
                                  (error "no elements in group" data))
                          (recur parent (cdr graph))))
                   ((text:)
                    ;; TODO: types
                    (cons (make-node (get-id-or-create data)
                                     text:
                                     (make-text (get data content:)
                                                (match (get+ data box2d:)
                                                       ((,a ,b) (make-box2d (apply make-vector2 a) (apply make-vector2 b))))
                                                (get data font:) ;; can be null so it uses the default of the (layer? tag?)
                                                (apply make-color (get data color:)))
                                     parent)
                          (recur parent (cdr graph))))
                   ((polyline:)
                    (cons (if-let [points (assq points: data)]
                                  (make-node (get-id-or-create data)
                                             polyline:
                                             (make-polyline (map (cut apply make-vector2 <>) (cdr points)) #f)
                                             parent)
                                  (error "no points in polyline:" data))
                          (recur parent (cdr graph))))
                   ((ref:)
                    (if-let [ref (table-ref objects (get data object:))]
                            ;; [[copy (u8vector->object (object->u8vector (car ref)))]]
                            (begin
                              (node-parent-set! (car ref) parent)
                              ref)
                            (error "reference does not exist:" ref)))
                   (else (error "unknown type in graph:" (car graph))))))))))

(define (scene-tree->scene-layers tree)
  (let recur ((tree tree))
    (if (null? tree)
        '()
        (let [(node (car tree))]
          (case (node-type node)
            ((text:)
             (render-layer-add-element 'default-layer node))
            ((polyline:)
             (render-layer-add-element 'default-layer node))
            ((group:)
             (recur (node-element node))))
          (recur (cdr tree)))))
  *render-layers*)

(define (render-scene-layers layers)
  (render-layers-for-each
   layers
   (lambda (id layer)
     ;; Render texts
     (with-gl-program
      'texture-2d
      (lambda (program-id)
        (with-text-render-state
         program-id
         (lambda () (for-each (compose text.render node-element) (render-layer-texts layer))))))

     ;; Render lines
     (with-gl-program
      'lines
      (lambda (program-id)
        (for-each
         (lambda (node)
           (-> node
               node-element
               polyline-vbo
               (vertex-object.render
                GL_LINE_STRIP
                (lambda ()
                  (check-gl-error
                   (glUniformMatrix4fv (glGetUniformLocation program-id "perspectiveMatrix") 1 GL_FALSE *gl-perspective-matrix*))
                  (let ((pos (glGetAttribLocation program-id "position")))
                    (glEnableVertexAttribArray pos)
                    (glVertexAttribPointer pos 2 GL_FLOAT GL_FALSE 0 #f))))))
         (render-layer-polylines layer)))))))

(define (render-graph graph)
  (-> graph
      (scene-data->scene-tree)
      (scene-tree->scene-layers)
      (render-scene-layers)))

(define (renderer:render)
  (glClearColor 0.0 0.0 0.0 1.0)
  (glClear GL_COLOR_BUFFER_BIT)

  (render-graph *scene-graph*)

  ;; Render debug texts
  (with-gl-program
   'texture-2d
   (lambda (program-id)
     (with-text-overlay-render-state
      program-id
      (lambda () (for-each text.render
                      (list (make-text (format "Scale factor: ~a" *scaling-factor*)
                                       (make-box2d (make-vector2 10.0 (- *screen-height* 40.0)) (make-vector2 100.0 100.0))
                                       '("assailand" 14)
                                       (make-color 255 255 255 255))
                            (make-text (format "Translation factor: [~a, ~a]" (vector2-x *translation-vector2*) (vector2-y *translation-vector2*))
                                       (make-box2d (make-vector2 10.0 (- *screen-height* 25.0)) (make-vector2 100.0 100.0))
                                       '("assailand" 14)
                                       (make-color 255 255 255 255)))))))))


;;
;; Fake data for testing
;;

(define (renderer:set-test-data!)
  (fonts:install "assailand" 14 "fonts/assailand/hinted-Assailand-Medium.ttf")
  (fonts:install "assailand" 34 "fonts/assailand/hinted-Assailand-Medium.ttf")

  (set! *scene-graph*
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
                         (points: (1 1) (0 0) (2 2) (3 3)))
              (polyline: (id: ,(uuid-v4))
                         (points: (3 2) (1 4) (5 2) (2 3)))
              (polyline: (id: ,(uuid-v4))
                         (points: (2 1) (4 4) (2 4) (0 3)))
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
             (box2d: (500.0 10.0) (40.0 34.0)))))))
