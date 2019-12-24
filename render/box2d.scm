(define-type box2d
  top-left dimensions)

(define (box2d-bottom-right b)
  (let ((tl (box2d-top-left b))
        (dim (box2d-dimensions b)))
   (make-vector2 (+ (vector2-x tl) (vector2-x dim))
                 (+ (vector2-y tl) (vector2-y dim)))))

