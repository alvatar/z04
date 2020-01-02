;; TODO: extract in library
(define (vector-swap! vec i j)
  (let [(x (vector-ref vec i))]
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j x)))

(define (vector-find f v)
  (let [(l (vector-length v))]
    (let loop [(idx 0)]
      (and (< idx l)
           (if-let [val (f (vector-ref v idx))]
                   val
                   (loop (+ idx 1)))))))

(define (vector-filter f v)
  (let* [(vlength (vector-length v))
         (newvec (make-vector vlength))]
    (let loop [(idx 0)
               (new-idx 0)]
      (if (< idx vlength)
          (let [(val (vector-ref v idx))]
            (if (f val)
                (begin (vector-set! newvec new-idx val)
                       (loop (+ idx 1)
                             (+ new-idx 1)))
                (loop (+ idx 1)
                      new-idx)))
          (begin (vector-shrink! newvec new-idx)
                 newvec)))))

;;
;; Render Tree
;;

(define-type node
  id: tree-node-type
  constructor: make-node/internal
  id type element
  (parent unprintable:)
  child-id
  (render-element unprintable:))

(define (make-node id type element parent child-id)
  ;; The render-element is set when creating render-layers
  (make-node/internal id type element parent child-id #f))

;;! Add Node. The node needs to have the parent defined so it's ready for insertion.
(define (scene-tree.add-node! node)
  (case (node-type (node-parent node))
    ((group: root:)
     (let* [(parent (node-parent node))
            (siblings (node-element parent))]
       (node-child-id-set! node (vector-length siblings))
       (node-element-set! parent (vector-append siblings (vector node)))
       ;; Add the element to a render layers
       ;; TODO: layer Id should be the current active layer
       (render-layer.add-element! *render-layers* 'default-layer node)))
    (else
     (error "node can only be added to a group or root"))))

;;! Modify a node with a new element
(define (scene-tree.alter-node! node element)
  (node-element-set! node element)
  (case (node-type node)
    ((text:) (text.refresh! element))
    ((polyline:) (polyline.refresh! element))
    (else
     (error "only text and polyline altering supported"))))

;;! Remove node
(define (scene-tree.remove-node! node)
  ;; TODO: when deleting many objects, it's cheaper to rebuild the list
  (case (node-type (node-parent node))
    ((group: root:)
     (let* [(parent (node-parent node))
            (siblings (node-element parent))
            (num-siblings (vector-length siblings))]
       ;; unless it's already the last child, swap it with the last one
       (let [(last-id (- num-siblings 1))
             (node-id (node-child-id node))]
         (unless (= node-id node-id last-id)
           (node-child-id-set! (vector-ref siblings last-id) node-id)
           (vector-swap! siblings node-id last-id)))
       (render-element.remove! (node-render-element node))
       (vector-shrink! siblings (- num-siblings 1))))
    (else
     (error "node can only be added to a group or root"))))

;;! Find element in children
(define (scene-tree.find-child node f)
  (vector-find (lambda (x) (and (f) x)) (node-element node)))

;;! Filter elements in children
(define (scene-tree.filter-children node f)
  (vector-filter f (node-element node)))

;;! Transform a graph into a renderable Scene Tree
(define (graph->scene-tree graph)
  (define objects (make-table))
  (define (get-id-or-create ls) (if-let [idl (assq id: ls)] (cadr idl) (uuid-v4)))
  (when-let
   [root (memq root: graph)]
   (let [(root-node (make-node "root" group: #f #f 0))]
     (node-element-set!
      root-node
      (list->vector
       (let recur ((parent root-node)
                   (graph (cdr root))
                   (child-id 0))
         (if (null? graph)
             '()
             (match (car graph)
                    ((objects: . ,data)
                     ;; TODO: Ignore references for now
                     ;; (for-each (lambda (obj) (table-set! objects (get obj id:) (recur parent (cdr obj) child-id))) data)
                     (recur parent (cdr graph) child-id))
                    ((group: . ,data)
                     (cons (if-let [elements (get+ data elements:)]
                                   (let [(node (make-node (get-id-or-create data) group: #f parent child-id))]
                                     (node-element-set! node (list->vector
                                                              (recur node elements 0)))
                                     node)
                                   (error "no elements in group" data))
                           (recur parent (cdr graph) (+ child-id 1))))
                    ((text: . ,data)
                     (cons (make-node (get-id-or-create data)
                                      text:
                                      (make-text (get data content:)
                                                 (match (get+ data box2d:)
                                                        ((,a ,b) (make-box2d (apply make-vector2 a) (apply make-vector2 b))))
                                                 (get data font:) ;; can be null so it uses the default of the (layer? tag?)
                                                 (apply make-color (get data color:)))
                                      parent
                                      child-id)
                           (recur parent (cdr graph) (+ child-id 1))))
                    ((polyline: . ,data)
                     (cons (if-let [points (assq points: data)]
                                   (make-node (get-id-or-create data)
                                              polyline:
                                              (make-polyline (map (cut apply make-vector2 <>) (cdr points)) #f)
                                              parent
                                              child-id)
                                   (error "no points in polyline:" data))
                           (recur parent (cdr graph) (+ child-id 1))))
                    ((ref: . ,data)
                     (error "references are not supported")
                     ;; (if-let [ref (table-ref objects (get data object:))]
                     ;;         (let [[copy (u8vector->object (object->u8vector (car ref)))]]
                     ;;           (node-parent-set! (car ref) parent)
                     ;;           ref)
                     ;;         (error "reference does not exist:" ref))
                     )
                    (else (error "unknown type in graph:" (car graph))))))))
     root-node)))
