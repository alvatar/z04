(define-macro (while condition . body)
  `(let loop ()
     (cond (,condition
	    (begin . ,body)
	    (loop)))))
