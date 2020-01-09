;;
;; GL Program
;;

(define *programs* (make-table)) ; name -> GLuint id

;;! Create a shader
;; .parameter Type of shader
;; .parameter Shader string
(define (gl-create-shader shader-type name shader-code)
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
            (error-log (string-append "GL Shading Language compilation in " name ": " (*->string info-log*))))))
    shader-id))

;;! Link a list of shaders
;; .parameter The shaders to link as a program
;; .parameter An optional callback receiving one argument (the program id) which will be invoked before linking
(define (gl-create-program shaders bind-callback)
  (let ((program-id (glCreateProgram))
        (program-status* (alloc-GLint* 1)))
    ;; Run bind-callback if provided
    (if bind-callback (bind-callback program-id))
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
(define (with-gl-program program-key f)
  (let ((program-id (table-ref *programs* program-key)))
    (glUseProgram program-id)
    (f program-id)
    (glUseProgram 0)))

(define (programs:init)
  ;; Lines program
  (let ((vertex-shader (gl-create-shader GL_VERTEX_SHADER "lines.vert" (load-text-file "assets/shaders/lines.vert")))
        (fragment-shader (gl-create-shader GL_FRAGMENT_SHADER "lines.frag" (load-text-file "assets/shaders/lines.frag"))))
    (table-set! *programs* 'lines
                (gl-create-program (list vertex-shader fragment-shader)
                                   identity)))
  ;; Texture 2d program
  (let ((vertex-shader (gl-create-shader GL_VERTEX_SHADER "tex2d.vert" (load-text-file "assets/shaders/tex2d.vert")))
        (fragment-shader (gl-create-shader GL_FRAGMENT_SHADER "tex2d.frag" (load-text-file "assets/shaders/tex2d.frag"))))
    (table-set! *programs* 'texture-2d
                (gl-create-program (list vertex-shader fragment-shader)
                                   (lambda (program-id)
                                     ;; TODO: 0 and 1 need to be used in glVertexAttribPointer in text.render.
                                     (glBindAttribLocation program-id 0 "position")
                                     (glBindAttribLocation program-id 1 "texCoord"))))))

(define (programs:shutdown)
  (table-for-each
   (lambda (name id) (glDeleteProgram id))
   *programs*))
