;;
;; Reader customization
;; It adds support for a-list based maps using {} syntax
;;

(include "~~lib/_gambit#.scm")

(define foreign-writers (make-table))
(define structure-writers (make-table))

(define (write-foreign we obj)
  (case (macro-writeenv-style we)
    ((mark)
     (##wr-mark we obj))
    (else
     (let ((tags (##foreign-tags obj)))
       (if (##pair? tags)
           ((table-ref foreign-writers (##car tags) ##wr-foreign) we obj)
           (##wr-foreign we obj))))))

(define (write-structure we obj)
  (case (macro-writeenv-style we)
    ((mark)
     (##wr-mark we obj))
    (else
     ((table-ref structure-writers (##structure-type obj) ##wr-structure)
   we obj)))
  )

(let ((old-wr ##wr))
  (set! ##wr
        (lambda (we obj)
          (cond ((##foreign? obj)
                 (write-foreign we obj))
                ((##structure? obj)
                 (write-structure we obj))
                (else
                 (old-wr we obj))))))

;; (table-set!
;;  foreign-writers
;;  'point*
;;  (lambda (we obj)
;;    (##wr-str we "{")
;;    (##wr we (point-x obj))
;;    (##wr-str we ",")
;;    (##wr we (point-y obj))
;;    (##wr-str we "}")))



(define-type reader-map
  id: reader-map-F9F00592-3D9F-468F-A10C-7260A6AA2DD7
  type-exhibitor: reader-map-type
  pairs)

(table-set!
 structure-writers
 (reader-map-type)
 (lambda (we obj) (reader:map-print we obj)))

(define (reader:make-map . args)
  (if (= 1 (length args))
      ;; The single-value function expects a map, and will produce a getter function
      (lambda (key #!optional opt) (or (reader:map-get (car args) key) opt))
      (make-reader-map
       (let loop ((m '())
                  (args args))
         (cond ((null? args)
                m)
               ((null? (cdr args))
                (error "maps must have an even number of elements"))
               ((assoc (car args) m)
                (error "maps cannot have repeated keys"))
               (else
                (loop (cons `(,(car args) . ,(cadr args)) m)
                      (cddr args))))))))

(define (reader:map-get m key)
  (let ((val (assoc key (reader-map-pairs m))))
    (if val
        (cdr val)
        #f)))

(define (reader:map-foreach f m)
  (let loop ((m (reader-map-pairs m)))
    (if (not (null? m))
        (begin (f (caar m) (cdar m))
               (loop (cdr m))))))

(define (reader:map-print we m)
  (##wr-str we "{")
  (let ((first? #t))
    (reader:map-foreach (lambda (key val)
                          (if (not first?) (##wr-str we ", "))
                          (##wr we key)
                          (##wr-str we " ")
                          (if (reader-map? val)
                              (reader:map-print we val)
                              (##wr we val))
                          (set! first? #f))
                        m))
  (##wr-str we "}"))

;; Alter reader
(##vector-set! ##main-readtable 30 'reader:make-map)

;; (pp {a: 1 b: 2})
;; (pp (reader:map-foreach (lambda (k v) (println k "---" v)) {a: 1}))

