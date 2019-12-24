(define *perspective-matrix* #f)
(define *gl-perspective-matrix* #f)
(define *screen-width* #f)
(define *screen-height* #f)

(define *fonts* (make-table))

(define *vertex-objects* #f)

;; A program is just a GL id
(define *programs* (make-table))

;; TODO: Transform scene-graph -> scene-tree

(define *scene-graph* #f)

(define *scene-tree* #f)

