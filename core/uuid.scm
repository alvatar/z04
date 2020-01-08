;; UUID v4 generator
;; Returns an RFC 4122, section 4.4 compliant uuid based on either the default random
;; function or one supplied with the function
(define (uuid-v4 #!optional [randfn random-integer])
  (define hex-vals (string->list "0123456789abcdef"))
  (define uuid-v4-pattern "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx")
  (define (x-replace) (list-ref hex-vals (randfn 16)))
  (define (y-replace) (list-ref hex-vals (bitwise-ior (bitwise-and (randfn 16) #x08) #x03)))
  (define (map-proc c) (cond
                        ((eq? c #\x) (x-replace))
                        ((eq? c #\y) (y-replace))
                        (else c)))
  (string-map map-proc uuid-v4-pattern))
