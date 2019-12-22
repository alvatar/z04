;;
;; Fonts
;;

(define-type font
  name size data)

;; Key: (name size)
(define *fonts* (make-table))

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
  dirty (texture unprintable:) (vbo unprintable:))

(define (text.update! text #!key :content :box2d :font :color)
  text)

(define (text.refresh! text)
  ;; TODO!
  text)

;;! text: string box2d font color
(define (make-text content box2d font-key color)
  (let ((font (table-ref *fonts* font-key)))
    (receive (texture-id* w h)
        (sdl-surface->gl-texture (TTF_RenderUTF8_Blended (font-data font) content (color->SDL_Color color)))
      (let* ((key (uuid-v4))
             (text (make-text/internal
                    content
                    box2d
                    font
                    color
                    #t
                    (make-texture/internal texture-id* key w h)
                    #f)))
        (text.refresh! text)
        text))))

(define (with-text-render-state f)
  (glEnable GL_TEXTURE_2D)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  ;; FILTER: Necessary for NPOT textures in GLES2
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  ;; WRAP: Necessary for NPOT textures in GLES2
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
  (f)
  (glDisable GL_TEXTURE_2D)
  (glDisable GL_BLEND))

(define (text.render text)
  'TODO
  ;; HERE >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  ;; Put below functionality here
  )
