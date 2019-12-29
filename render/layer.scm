(define-type render-layer
  constructor: make-render-layer/internal
  texts
  texts-end ; tail for O(1) appending
  polylines
  polylines-end ; idem
  )

(define (make-render-layer)
  (make-render-layer/internal #f #f #f #f))

(define (make-render-layers) (make-hash-table))

;; Layers have the structure (list list-tail) to support O(1) append
(define *render-layers* #f)

(define (render-layers-clear! layers) (set! layers (make-hash-table)))

(define (render-layer-add-element layers layer-id node)
  (hash-table-update!/default
   layers
   layer-id
   ;; This updater function uses the tail so we can directly append in O(1)
   (lambda (layer) (let [(new (list node))]
                ;; (node-parent-set! node #f) ;; Remove parent links
                (case (node-type node)
                  ((text:)
                   (if-let [end (render-layer-texts-end layer)]
                           (begin (set-cdr! end new)
                                  (node-previous-set! node (car end)))
                           (render-layer-texts-set! layer new))
                   (render-layer-texts-end-set! layer new))
                  ((polyline:)
                   (if-let [end (render-layer-polylines-end layer)]
                           (begin (set-cdr! end new)
                                  (node-previous-set! node (car end)))
                           (render-layer-polylines-set! layer new))
                   (render-layer-polylines-end-set! layer new)))
                layer))
   (make-render-layer)))

(define (render-layers-get layers layer-id)
  (when-let [l (hash-table-ref layers layer-id #f)]
            (car l)))

(define (render-layers-for-each layers f)
  (hash-table-walk layers f))

(define (render-layers.render layers)
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
                  ;;
                  ;; TODO MOVE
                  ;;
                  (check-gl-error
                   (glUniformMatrix4fv (glGetUniformLocation program-id "perspectiveMatrix") 1 GL_FALSE *gl-perspective-matrix*))
                  (let ((pos (glGetAttribLocation program-id "position")))
                    (glEnableVertexAttribArray pos)
                    (glVertexAttribPointer pos 2 GL_FLOAT GL_FALSE 0 #f))))))
         (render-layer-polylines layer)))))))
