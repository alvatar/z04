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

(define (error-log msg) (println (string-append "ERROR in GL: " msg)))

;; Executes the given form and checks if GL's state is valid
(define-macro (check-gl-error exp)
  `(let* ((result ,exp))
     (let loop [(err (glGetError))]
       (unless (= err GL_NO_ERROR)
         (let [(errstr
                (cond
                 ((= err GL_INVALID_ENUM) "GL_INVALID_ENUM")
                 ((= err GL_INVALID_VALUE) "GL_INVALID_VALUE")
                 ((= err GL_INVALID_OPERATION) "GL_INVALID_OPERATION")
                 ((= err GL_INVALID_FRAMEBUFFER_OPERATION) "GL_INVALID_FRAMEBUFFER_OPERATION")
                 ((= err GL_OUT_OF_MEMORY) "GL_OUT_OF_MEMORY")))]
           (error-log (string-append "GL Error -- " errstr
                                     " - " (object->string ',exp))))
         (loop (glGetError))))
     result))
