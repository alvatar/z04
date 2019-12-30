;;
;; Style (color, line, fill)
;;

(define-type style
  line-color
  line-type
  line-width
  fill-color
  )

;; TODO: color
(define *default-style* (make-style (make-color 1. 1. 1. 1.) 'solid 1. (make-color 1. 1. 1. 1.)))

(define (get-default-style) *default-style*)
