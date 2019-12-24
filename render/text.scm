;;
;; Fonts
;;

(define-type font
  name size data)

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
  (TTF_Init)
  (fonts:install "assailand" 14 "fonts/assailand/hinted-Assailand-Medium.ttf"))

(define (fonts:shutdown)
  (table-for-each
   (lambda (key data) (fonts:uninstall (car key) (cadr key)))
   *fonts*)
  (TTF_Quit))

;;
;; Text
;;

(define-type text
  constructor: make-text/internal
  content box2d font color
  dirty? texture (vertex-object unprintable:))

(define (text.update! text #!key :content :box2d :font :color)
  text)

(define (box2d->f32vector-path x1 y1 w h)
  (let ((qx1 x1)
        (qy1 y1))
    (let ((qx2 (+ qx1 w))
          (qy2 (+ qy1 h)))
      (f32vector qx1 qy1 0.0 0.0
                 qx1 qy2 0.0 1.0
                 qx2 qy1 1.0 0.0
                 qx2 qy1 1.0 0.0
                 qx1 qy2 0.0 1.0
                 qx2 qy2 1.0 1.0))))

(define (text.refresh! text)
  ;; TODO: remove old texture
  (when (text-dirty? text)
    (let ((font (table-ref *fonts* (text-font text)))
          (key (uuid-v4)))
      (receive (texture-id w h)
          (sdl-surface->gl-texture
           (TTF_RenderUTF8_Blended (font-data font) (text-content text) (color->SDL_Color (text-color text))))
        (text-dirty?-set! text #f)
        (text-texture-set! text texture-id)
        ;; TODO: just update vertex object if necessary, use proper values
        (text-vertex-object-set! text (make-vertex-object
                                       ;; TODO!
                                       (box2d->f32vector-path 100.0 100.0 w h) #t #f))
        text)))
  text)

;;! text: string box2d font-key color
(define (make-text content box2d font-key color)
  (let ((text (make-text/internal
               content
               box2d
               font-key
               color
               #t
               #f
               #f)))
    (text.refresh! text)
    text))

(define (with-text-render-state f)
  (glEnable GL_TEXTURE_2D)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (f)
  (glDisable GL_TEXTURE_2D)
  (glDisable GL_BLEND)
  )

(define (text.render text program-id)
  (let ((vo (text-vertex-object text)))
    (gl-draw-with-vbo
     (vertex-object-vbo vo)
     GL_TRIANGLES
     6
     (lambda ()
       (check-gl-error
        (glUniform1i (glGetUniformLocation program-id "colorTexture") 0))
       (check-gl-error
        (glUniformMatrix4fv (glGetUniformLocation program-id "perspectiveMatrix") 1 GL_FALSE *gl-perspective-matrix*))
       (glEnableVertexAttribArray 0)
       (glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE (* 4 GLfloat-size) #f)
       (glEnableVertexAttribArray 1)
       (glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (* 4 GLfloat-size) (integer->void* (* 2 GLfloat-size)))
       (glActiveTexture GL_TEXTURE0)
       (glBindTexture GL_TEXTURE_2D (text-texture text))))))