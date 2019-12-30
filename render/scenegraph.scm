;;
;; SceneGraph and SceneTree
;;
;; A Scene graph is a list with internal references made of simple definitions of geometrical
;; objects. It is transformed into a tree, with full information for rendering and transformations.
;; This tree is made of nodes, starting from a root node (root:)
;;

(define-type node
  id: graph-node-type
  constructor: make-node/internal
  id type element
  dirty
  (parent unprintable:)
  render-element)

(define (make-node id type element parent)
  (make-node/internal id type element #f parent #f))

(define (graph->layers graph)
  (-> graph scene-data->scene-tree scene-tree->scene-layers))

(define (scene-data->scene-tree graph)
  (define objects (make-table))
  (define (get-id-or-create ls) (if-let [idl (assq id: ls)] (cadr idl) (uuid-v4)))
  (when-let
   [root (memq root: graph)]
   (let recur ((parent graph)
               (graph (cdr root)))
     (if (null? graph)
         '()
         (match (car graph)
                ((objects: . ,data)
                 (for-each (lambda (obj) (table-set! objects (get obj id:) (recur parent (cdr obj)))) data)
                 (recur parent (cdr graph)))
                ((group: . ,data)
                 (cons (if-let [elements (get+ data elements:)]
                               (let [(node (make-node (get-id-or-create data) group: #f parent))]
                                 (node-element-set! node (recur node elements))
                                 node)
                               (error "no elements in group" data))
                       (recur parent (cdr graph))))
                ((text: . ,data)
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
                ((polyline: . ,data)
                 (cons (if-let [points (assq points: data)]
                               (make-node (get-id-or-create data)
                                          polyline:
                                          (make-polyline (map (cut apply make-vector2 <>) (cdr points)) #f)
                                          parent)
                               (error "no points in polyline:" data))
                       (recur parent (cdr graph))))
                ((ref: . ,data)
                 (if-let [ref (table-ref objects (get data object:))]
                         ;; [[copy (u8vector->object (object->u8vector (car ref)))]]
                         (begin
                           (node-parent-set! (car ref) parent)
                           ref)
                         (error "reference does not exist:" ref)))
                (else (error "unknown type in graph:" (car graph))))))))

(define (scene-tree.add-node node)
  (unless (or (eq? (node-type (node-parent node)) group:)
              (eq? (node-parent node) root:))
    (error "node can only be added to a group"))
  (match (node-parent node)
         ((root: . ,data)
          'TODO-ADD)
         ((group: . ,data)
          'TODO-ADD-TO-ELEMENT)))

(define (scene-tree.remove-node node)
  ;; TODO: when deleting many objects, it's cheaper to rebuild the list
  'TODO)

(define (scene-tree->scene-layers tree)
  (let [(layers (make-render-layers))]
    (let recur ((tree tree))
      (if (null? tree)
          '()
          (let [(node (car tree))]
            (case (node-type node)
              ((text:)
               (render-layer.add-element layers 'default-layer node))
              ((polyline:)
               (render-layer.add-element layers 'default-layer node))
              ((group:)
               (recur (node-element node))))
            (recur (cdr tree)))))
    layers))
