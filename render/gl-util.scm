(define (error-log msg)
  (display (string-append "ERROR: " msg)))

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

;; Executes the given form and checks if GL's state is valid
(define-macro (check-gl-error exp)
  `(let* ((result ,exp)
          (error (glGetError)))
     (unless (= error GL_NO_ERROR)
             (error-log (string-append "GL Error -- " (object->string error)
                                       " - " (object->string ',exp))))
     result))

;; ;; Loads an image from the given path and creates a texture object to hold it
;; (define (load-texture->gl-texture window path)
;;   (let* ((texture-img* (IMG_Load path)) ;; default format: ARGB8888
;;          (texture-id* (alloc-GLuint* 1)))
;;     ;; Alternative method (using GL_RGBA). Remember that PixelFormat is backwards in SDL
;;     ;; (SDL_ConvertSurfaceFormat texture-img-unformatted* SDL_PIXELFORMAT_ABGR8888 0)
;;     ;; Generate and bind texture
;;     (glGenTextures 1 texture-id*)
;;     (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id*))
;;     ;; Check errors
;;     (check-gl-error
;;      (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA ; internal format
;;                    (SDL_Surface-w texture-img*) (SDL_Surface-h texture-img*)
;;                    0 (cond-expand (ios GL_BGRA_EXT) (else GL_BGRA)) GL_UNSIGNED_BYTE
;;                    (SDL_Surface-pixels texture-img*)))
;;     ;; FILTER: Necessary for NPOT textures in GLES2
;;     (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
;;     (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
;;     ;; WRAP: Necessary for NPOT textures in GLES2
;;     (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
;;     (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
;;     ;; Unbind and free the surface
;;     (glBindTexture GL_TEXTURE_2D 0)
;;     (SDL_FreeSurface texture-img*)
;;     texture-id*))

;;! Create a shader
;; .parameter Type of shader
;; .parameter Shader string
(define (gl-create-shader shader-type shader-code)
  (let ((shader-id (glCreateShader shader-type))
        (shader-status* (alloc-GLint* 1)))
    (glShaderSource shader-id 1 (list shader-code) #f)
    (glCompileShader shader-id)
    (glGetShaderiv shader-id GL_COMPILE_STATUS shader-status*)
    (if (= GL_FALSE (*->GLint shader-status*))
        (let ((info-log-length* (alloc-GLint* 1)))
          (glGetShaderiv shader-id GL_INFO_LOG_LENGTH info-log-length*)
          (let* ((info-log-length (*->GLint info-log-length*))
                 (info-log* (alloc-GLchar* info-log-length)))
            (glGetShaderInfoLog shader-id info-log-length #f info-log*)
            (error-log (string-append "GL Shading Language compilation -- " (*->string info-log*))))))
    shader-id))

;;!! Link a list of shaders
;; .parameter The shaders to link as a program
;; .parameter An optional callback receiving one argument (the program id) which will be invoked before linking
(define (gl-create-program shaders)
  (let ((program-id (glCreateProgram))
        (program-status* (alloc-GLint* 1)))
    ;; Run bind-callback if provided
    ;; (if bind-callback (bind-callback program-id))
    ;; Link shader
    (for-each (lambda (s) (glAttachShader program-id s)) shaders)
    (glLinkProgram program-id)
    (glGetProgramiv program-id GL_LINK_STATUS program-status*)
    (if (= GL_FALSE (*->GLint program-status*))
        (let ((info-log-length* (alloc-GLint* 1)))
          (glGetProgramiv program-id GL_INFO_LOG_LENGTH info-log-length*)
          (let* ((info-log-length (*->GLint info-log-length*))
                 (info-log* (alloc-GLchar* info-log-length)))
            (glGetProgramInfoLog program-id info-log-length #f info-log*)
            (error-log (string-append "GL Shading Language linkage -- " (*->string info-log*))))))
    (for-each (lambda (s) (glDetachShader program-id s)) shaders)
    ;; (if delete-shaders?(for-each glDeleteShader shaders))
    program-id))

;; Creates a new OpenGL VBO from a given f32vector.
(define (f32vector->gl-buffer vertex-data-vector buffer-type)
  (let ((buffer-id* (alloc-GLuint* 1)))
    (glGenBuffers 1 buffer-id*)
    (glBindBuffer GL_ARRAY_BUFFER (*->GLuint buffer-id*))
    (glBufferData GL_ARRAY_BUFFER
                  (* (f32vector-length vertex-data-vector) GLfloat-size)
                  (f32vector->GLfloat* vertex-data-vector)
                  buffer-type)
    (glBindBuffer GL_ARRAY_BUFFER 0)
    buffer-id*))

(define (with-program program-id f)
  (glUseProgram program-id)
  (f)
  (glUseProgram 0))

;; Draws the given vbo with a particular program. The callback is
;; used to set up the attributes of the dynamic attributes
(define (gl-draw-with-vbo vbo-id* type count attribs-callback)
  (let ((vbo-id (*->GLuint vbo-id*)))
    (when (check-gl-error (glBindBuffer GL_ARRAY_BUFFER vbo-id))
      (attribs-callback)
      (check-gl-error (glDrawArrays type 0 count))
      (glBindBuffer GL_ARRAY_BUFFER 0))))
