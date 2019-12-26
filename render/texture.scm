;;
;; Textures
;;

;;! Type: Texture
(define-type texture
  constructor: make-texture/internal
  id  ;; the OpenGL identifier
  key ;; a symbol identifier
  width
  height)

;;! Textures are automatically registered and can be later accessed from the global table
;; (define (load-texture key path)
;;   (define (load-texture->gl-texture path)
;;     (let ((texture-img* (IMG_Load path)) ;; default format: ARGB8888
;;           (texture-id* (alloc-GLuint* 1)))
;;       (unless texture-img* (error-log (IMG_GetError)))
;;       (let ((texture-height (SDL_Surface-h texture-img*))
;;             (texture-width (SDL_Surface-w texture-img*)))
;;         ;; Alternative method (using GL_RGBA). Remember that PixelFormat is backwards in SDL
;;         ;; (SDL_ConvertSurfaceFormat texture-img-unformatted* SDL_PIXELFORMAT_ABGR8888 0)
;;         ;; Generate and bind texture
;;         (glGenTextures 1 texture-id*)
;;         (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id*))
;;         ;; Check errors
;;         (check-gl-error
;;          (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA ; internal format
;;                        texture-width texture-height
;;                        0 (cond-expand (ios GL_BGRA_EXT) (else GL_BGRA)) GL_UNSIGNED_BYTE
;;                        (SDL_Surface-pixels texture-img*)))
;;         ;; FILTER: Necessary for NPOT textures in GLES2
;;         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
;;         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
;;         ;; WRAP: Necessary for NPOT textures in GLES2
;;         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
;;         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
;;         ;; Unbind and free the surface
;;         (glBindTexture GL_TEXTURE_2D 0)
;;         (SDL_FreeSurface texture-img*)
;;         (values texture-id* texture-width texture-height))))
;;   (receive (texture-id* w h)
;;       (load-texture->gl-texture path)
;;     (let ((instance (make-texture/internal texture-id* key w h)))
;;       (table-set! *gl-textures* key instance)
;;       instance)))

(define (sdl-surface->gl-texture texture-surf*) ;; default format: ARGB8888
  (unless texture-surf* (raise "No surface provided"))
  (let ((texture-id* (alloc-GLuint* 1))
        (texture-height (SDL_Surface-h texture-surf*))
        (texture-width (SDL_Surface-w texture-surf*)))
    ;; Alternative method (using GL_RGBA). Remember that PixelFormat is backwards in SDL
    ;; (SDL_ConvertSurfaceFormat texture-img-unformatted* SDL_PIXELFORMAT_ABGR8888 0)
    ;; Generate and bind texture
    (glGenTextures 1 texture-id*)
    (let ((texture-id (*->GLuint texture-id*)))
      (glBindTexture GL_TEXTURE_2D texture-id)
      ;; FILTER: Necessary for NPOT textures in GLES2
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
      ;; WRAP: Necessary for NPOT textures in GLES2
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
      ;; Check errors
      (check-gl-error
       (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA ; internal format
                     texture-width texture-height
                     0 GL_BGRA_EXT GL_UNSIGNED_BYTE
                     (SDL_Surface-pixels texture-surf*)))
      ;; Unbind and free the surface
      (glBindTexture GL_TEXTURE_2D 0)
      (SDL_FreeSurface texture-surf*)
      (values texture-id texture-width texture-height))))
