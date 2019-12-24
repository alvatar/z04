;;!! Loads a text file from the given path. Returns a string with the contents or #f if the file does not exist
;; .parameter The path of the file to load
;; (define (load-text-file path)
;;   (and-let* ((rw (SDL_RWFromFile path "rt"))
;;              (file-size (SDL_RWsize rw))
;;              (buffer (alloc-char* (+ 1 file-size)))
;;              (bytes-read (SDL_RWread rw (*->void* buffer) 1 file-size)))
;;             (SDL_RWclose rw)
;;             (char*-set! buffer file-size #\nul)
;;             (*->string buffer)))
(define (load-text-file file)
  (with-input-from-file file
    (lambda () (read-line (current-input-port) #!eof))))

(define (error-log msg) (display (string-append "ERROR in GL: " msg)))

;; Executes the given form and checks if GL's state is valid
(define-macro (check-gl-error exp)
  `(let* ((result ,exp)
          (error (glGetError)))
     (unless (= error GL_NO_ERROR)
       (error-log (string-append "GL Error -- " (object->string error)
                                 " - " (object->string ',exp))))
     result))

;; UUID v4 generator
;; Returns an RFC 4122, section 4.4 compliant uuid based on either the default random
;; function or one supplied with the function
(define (uuid-v4 #!optional [randfn random-integer])
  (define hex-vals (string->list "0123456789abcdef"))
  (define uuid-v4-pattern "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx")
  (letrec ([x-replace (lambda () (list-ref hex-vals (randfn 16)))]
           [y-replace (lambda () (list-ref hex-vals (bitwise-ior (bitwise-and (randfn 16) #x08) #x03)))]
           [map-proc (lambda (c)
                       (cond
                        ((eq? c #\x) (x-replace))
                        ((eq? c #\y) (y-replace))
                        (else c)))])
    (string-map map-proc uuid-v4-pattern)))
