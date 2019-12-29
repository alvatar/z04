;;
;; Render Element
;;

(define-type render-element
  node
  (sublayer unprintable:)
  previous
  next)

(define (render-element.add sublayer re)
  (if-let [end (sublayer-last sublayer)]
          (begin
            (render-element-previous-set! re end)
            (render-element-next-set! end re))
          (sublayer-first-set! sublayer re))
  (sublayer-last-set! sublayer re))

(define (render-element.remove re)
  (let [(sl (render-element-sublayer render-element))]
    (cond
     ;; Element is first in render layer
     ((not (render-element-previous re))
      (if-let [next (render-element-next re)]
              ;; Element is first but there are more
              (begin (sublayer-first-set! sl next)
                     (render-element-previous-set! next #f))
              ;; Element was unique
              (begin
                (sublayer-first-set! sl #f)
                (sublayer-last-set! sl #f))))
     ;; Element is last in render layer
     ((not (render-element-next re))
      (let [(previous (render-element-previous re))]
        (render-element-next-set! previous #f)
        (sublayer-last-set! sl previous)))
     ;; Element is in the middle
     (else
      (let [(previous (render-element-previous re))
            (next (render-element-next re))]
        (render-element-next-set! previous next)
        (render-element-previous-set! next previous))))))

;;
;; Render Layer
;;
;; A layer is comprised of sublayers. Each sublayer is a doubly-linked list of render-element

(define-type sublayer
  constructor: make-sublayer/internal
  first last)

(define (make-sublayer)
  (make-sublayer/internal #f #f))

(define-type render-layer
  constructor: make-render-layer/internal
  texts-sublayer                        ; text sub-layer
  polylines-sublayer                    ; polylines sub-layer
  )

(define (make-render-layer)
  (make-render-layer/internal (make-sublayer) (make-sublayer)))

(define (render-layer.add-element layers layer-id node)
  (hash-table-update!/default
   layers
   layer-id
   ;; This updater function uses the tail so we can directly append in O(1)
   (lambda (layer) (let [(new (make-render-element node layer #f #f))]
                (node-render-element-set! node new)
                (case (node-type node)
                  ((text:) (render-element.add (render-layer-texts-sublayer layer) new))
                  ((polyline:) (render-element.add (render-layer-polylines-sublayer layer) new)))
                layer))
   (make-render-layer)))

(define (render-layer.for-each-sub f sub)
  (let recur [(e-head sub)]
    (when e-head
      (f e-head)
      (recur (render-element-next e-head)))))

;;
;; Render Layers structure
;;

(define (make-render-layers) (make-hash-table))

(define (render-layers.clear! layers) (set! layers (make-hash-table)))

(define (render-layers.get layers layer-id)
  (when-let [l (hash-table-ref layers layer-id #f)]
            (car l)))

(define (render-layers.for-each layers f)
  (hash-table-walk layers f))

(define (render-layers.render layers)
  (render-layers.for-each
   layers
   (lambda (id layer)
     ;; Render texts
     (with-gl-program
      'texture-2d
      (lambda (program-id)
        (with-text-render-state
         program-id
         (lambda ()
           (render-layer.for-each-sub (compose text.render node-element render-element-node)
                                      (sublayer-first (render-layer-texts-sublayer layer)))))))
     ;; Render lines
     (with-gl-program
      'lines
      (lambda (program-id)
        (with-polyline-render-state
         program-id
         (lambda ()
           (render-layer.for-each-sub (compose polyline.render node-element render-element-node)
                                      (sublayer-first (render-layer-polylines-sublayer layer))))))))))
