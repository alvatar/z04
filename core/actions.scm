(define action:quit 1)
(define action:test 2)
(define action:translate-space 100)
(define action:scale-space 101)
(define action:start-polyline 1000)

(define action-listeners (make-hash-table))

(define (add-action-listener action fn)
  (hash-table-update!/default action-listeners
                              action
                              (lambda (l) (cons (lambda (data)
                                             (if (string? data)
                                                 (-> data json-decode fn)
                                                 (fn data)))
                                           l))
                              '()))


(define (get-action-listeners action)
  (hash-table-ref/default action-listeners action '()))

