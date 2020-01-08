;;
;; Fonts
;;

;;
;; TODO: real font renderer
;; https://github.com/akrinke/Font-Stash
;; https://github.com/grimfang4/SDL_FontCache
;; https://github.com/nothings/stb
;;

(define-type font
  name size data)

(define *fonts* (make-table)) ; (name size) -> font

;; Key: (name size)

(define (fonts:install name size file)
  (let ((key (list name size)))
    (unless (table-ref *fonts* key #f)
      (table-set! *fonts* key (make-font name size (TTF_OpenFont file size)))
      (println "Installed font: " name " " size "pt"))))

(define (fonts:uninstall font size)
  (let ((key (list font size)))
    (when-let [ff (table-ref *fonts* key #f)]
              (TTF_CloseFont (font-data ff))
              (table-set! *fonts* key))))

(define (fonts:uninstall-by-name name)
  (table-for-each
   (lambda (key data)
     (when (string=? name (car key))
       (fonts:uninstall (car key) (cadr key))))
   *fonts*))

(define (fonts:init)
  (TTF_Init))

(define (fonts:shutdown)
  (table-for-each
   (lambda (key data) (fonts:uninstall (car key) (cadr key)))
   *fonts*)
  (TTF_Quit))

;;
;; Text
;;

(define-type text
  id: text-type
  constructor: make-text/internal
  content box2d font color
  dirty? texture (vertex-object unprintable:))

;;! text: string box2d font-key color
(define (make-text content box2d font-key color)
  (let ((text (make-text/internal content box2d font-key color #t #f #f)))
    (text.refresh! text)
    text))

(define (text.refresh! text #!optional force?)
  (when (or (text-dirty? text) force?)
    (let ((font (table-ref *fonts* (text-font text)))
          (key (uuid-v4))
          (box2d (text-box2d text)))
      (receive (texture-id w h)
          (sdl-surface->gl-texture
           (TTF_RenderUTF8_Blended (font-data font) (text-content text) (color->SDL_Color (text-color text))))
        (when-let [old-texture (text-texture text)]
                  (glDeleteTextures 1 old-texture))
        (text-texture-set! text texture-id)
        (text-vertex-object-set! text
                                 (make-vertex-object
                                  (texture-vertices-rect
                                   (box2d-top-left box2d)
                                   (vector2+ (box2d-top-left box2d)
                                             ;; TODO: Multiplied by 0.5 for HDPI. Revisit.
                                             (vector2*scalar (make-vector2 w h)
                                                             0.5)))))
        (text-dirty?-set! text #f)
        text)))
  text)

(define (text.update! text #!key content box2d font color)
  (when content (text-content-set! text content))
  (when box2d (text-box2d-set! text box2d))
  (when font (text-font-set! text font))
  (when color (text-color-set! text color))
  (text.refresh! text #t))

(define (texture-vertices-rect top-left bottom-right)
  (let ((qx1 (vector2-x top-left))
        (qy1 (vector2-y top-left))
        (qx2 (vector2-x bottom-right))
        (qy2 (vector2-y bottom-right)))
    (f32vector qx1 qy1 0.0 0.0
               qx1 qy2 0.0 1.0
               qx2 qy1 1.0 0.0
               qx2 qy1 1.0 0.0
               qx1 qy2 0.0 1.0
               qx2 qy2 1.0 1.0)))

(define (with-text-render-state/aux program-id f0 matrix)
  (glEnable GL_TEXTURE_2D)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (check-gl-error
   (glUniform1i (glGetUniformLocation program-id "colorTexture") 0))
  (check-gl-error
   (glUniformMatrix4fv (glGetUniformLocation program-id "perspectiveMatrix") 1 GL_FALSE matrix))
  (f0)
  (glDisable GL_TEXTURE_2D)
  (glDisable GL_BLEND))

(define (with-text-render-state program-id f0)
  (with-text-render-state/aux program-id f0 *gl-perspective-matrix*))

(define (with-text-overlay-render-state program-id f0)
  (with-text-render-state/aux program-id f0 *gl-base-matrix*))

;;! Text render must be run within a with-text-* function
(define (text.render text)
  (-> text
      text-vertex-object
      (vertex-object.render
       GL_TRIANGLES
       (lambda ()
         ;; (glGetAttribLocation program-id "position")
         ;; attribute vec2 position = 0
         ;; attribute vec2 texCoord = 1
         (glEnableVertexAttribArray 0)
         (glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE (* 4 GLfloat-size) #f)
         (glEnableVertexAttribArray 1)
         (glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (* 4 GLfloat-size) (integer->void* (* 2 GLfloat-size)))
         (glActiveTexture GL_TEXTURE0)
         (glBindTexture GL_TEXTURE_2D (text-texture text))))))
