(define-type render-layer
  constructor: make-render-layer/internal
  texts
  texts-end ; tail for O(1) appending
  polylines
  polylines-end ; idem
  )

(define (make-render-layer)
  (make-render-layer/internal #f #f #f #f))

;; Layers have the structure (list list-tail) to support O(1) append
(define *render-layers* (make-hash-table))

(define (render-layer-add-element layer-id node)
  (hash-table-update!/default
   *render-layers* layer-id
   ;; This updater function uses the tail so we can directly append in O(1)
   (lambda (layer) (let [(new (list node))]
                (node-parent-set! node #f) ;; Remove parent links
                (case (node-type node)
                  ((text:)
                   (if-let [end (render-layer-texts-end layer)]
                           (set-cdr! (render-layer-texts-end layer) new)
                           (render-layer-texts-set! layer new))
                   (render-layer-texts-end-set! layer new))
                  ((polyline:)
                   (if-let [end (render-layer-polylines-end layer)]
                           (set-cdr! (render-layer-polylines-end layer) new)
                           (render-layer-polylines-set! layer new))
                   (render-layer-polylines-end-set! layer new)))
                layer))
   (make-render-layer)))

(define (render-layers-get layers layer-id)
  (when-let [l (hash-table-ref layers layer-id #f)]
            (car l)))

(define (render-layers-for-each layers f)
  (hash-table-walk layers f))
