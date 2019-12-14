(define-library (base)

  (import gambit)

  (export unless)

  (begin

    ;;! unless
    ;; The opposite of when
    ;; Equivalent low-level macro:
    ;; (##define-macro (unless . args)
    ;;   (let ((condition (car args))
    ;;         (forms (cdr args)))
    ;;     `(or ,condition (begin ,@forms))))
    (define-syntax unless
      (syntax-rules ()
        ((_ ?test ?form . ?forms)
         (if ?test #f (begin ?form . ?forms)))))))
