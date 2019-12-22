;;
;; GL Program
;;

;; A program is just a GL id
(define *programs* (make-table))

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

;;! Link a list of shaders
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

;;! Execute thunk with a GL program
(define (with-gl-program program-id f)
  (glUseProgram program-id)
  (f program-id)
  (glUseProgram 0))

;; (define vertex-shader-source
;; "
;; attribute vec4 position;
;; void main()
;; {
;;     gl_Position = vec4(position.xyz, 1.0);
;; }"
;; )

;; (define fragment-shader-source
;; "
;; void main()
;; {
;;     gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
;; }"
;; )

(define (programs:init)
  ;; Lines program
  (let ((vertex-shader (gl-create-shader GL_VERTEX_SHADER (load-text-file "render/shaders/lines.vert")))
        (fragment-shader (gl-create-shader GL_FRAGMENT_SHADER (load-text-file "render/shaders/lines.frag"))))
    (table-set! *programs* 'lines (gl-create-program (list vertex-shader fragment-shader))))
  ;; Texture 2d program
  (let ((vertex-shader (gl-create-shader GL_VERTEX_SHADER (load-text-file "render/shaders/tex2d.vert")))
        (fragment-shader (gl-create-shader GL_FRAGMENT_SHADER (load-text-file "render/shaders/tex2d.frag"))))
    (table-set! *programs* 'texture-2d (gl-create-program (list vertex-shader fragment-shader)))))

(define (programs:shutdown)
  (table-for-each
   (lambda (name id) (glDeleteProgram id))
   *programs*))
