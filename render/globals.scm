;; Space geometry and projection
(define *screen-width*)
(define *screen-height*)
(define *base-matrix*)
(define *gl-base-matrix*)

(define *translation-vector2* (make-vector2 0.0 0.0))
(define *scaling-factor* 1.0)
(define *perspective-matrix*)
(define *gl-perspective-matrix*)


;; Scene
(define *scene-graph*)
(define *scene-tree*)
(define *vertex-objects*)

;; Resources
(define *fonts* (make-table)) ; (name size) -> font
(define *programs* (make-table)) ; name -> GLuint id

;; Misc
(define *debug-texts*)
