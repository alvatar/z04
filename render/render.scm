(import (github.com/alvatar/ffi-utils))
(import (github.com/alvatar/sdl2 ttf))


(include "gl-util.scm")

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

(define-type vertex-object vertices dirty vbo)

;;
;; Global GL objects
;;

(define *vertex-objects*
  (list
   (make-vertex-object (f32vector 0.0 0.5 0.5 -0.5 -0.5 -0.5) #t #f)
   (make-vertex-object (f32vector 1.0 1.0 1.5 -1.5 -1.5 -1.5) #t #f)
   (make-vertex-object (f32vector 2.0 -0.5 1.5 -1.5 -0.5 1.5) #t #f)))

(define *programs* (make-table))

(define *fonts* (make-table))

;;
;; Fonts
;;

(define (fonts:init)
  (TTF_Init)
  (let ((assailand-font (make-table)))
    (table-set! assailand-font 14 (TTF_OpenFont "fonts/assailand/hinted-Assailand-Medium.ttf" 14))
    (table-set! *fonts* "assailand" assailand-font)))

(define (fonts:shutdown)
  (table-for-each
   (lambda (name font)
     (table-for-each (lambda (font font-data) (TTF_CloseFont font-data)) font))
   *fonts*)
  (TTF_Quit))

;;
;; Programs
;;

(define (programs:init)
  ;; Lines program
  (let ((vertex-shader (gl-create-shader GL_VERTEX_SHADER (load-text-file "render/shaders/lines.vert")))
        (fragment-shader (gl-create-shader GL_FRAGMENT_SHADER (load-text-file "render/shaders/lines.frag"))))
    (table-set! *programs* 'lines (gl-create-program (list vertex-shader fragment-shader))))
  ;; Texture 2d program
  (let ((vertex-shader (gl-create-shader GL_VERTEX_SHADER (load-text-file "render/shaders/tex2d.vert")))
        (fragment-shader (gl-create-shader GL_FRAGMENT_SHADER (load-text-file "render/shaders/tex2d.frag"))))
    (table-set! *programs* 'tex2d (gl-create-program (list vertex-shader fragment-shader)))))

(define (programs:shutdown)
  'TODO
  )

;;
;; Renderer
;;

(define (renderer:init)
  (programs:init)
  (fonts:init))

(define (renderer:shutdown)
  (fonts:shutdown)
  (programs:shutdown))

(define (render-text x y text)
  (let ((color (alloc-SDL_Color)))
    (SDL_Color-r-set! color 200)

    (glDisable GL_DEPTH_TEST)
    (glEnable GL_TEXTURE_2D)
    (glEnable GL_BLEND)
    (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

    (let ((surface (TTF_RenderUTF8_Blended *font* text (*->SDL_Color color)))
          (texture-id (alloc-GLuint* 1)))
      (let ((w (SDL_Surface-w surface))
            (h (SDL_Surface-h surface)))
        (glGenTextures 1 texture-id)
        (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id))

        (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
        (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
        (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA w h 0 GL_BGRA_EXT GL_UNSIGNED_BYTE (SDL_Surface-pixels surface))

        (glBegin GL_QUADS)
        (glTextCoord2f 0 0)
        (glTextCoord2f 1 0)
        (glTextCoord2f 1 1)
        (glTextCoord2f 0 1)
        (glEnd)

        (glDisable GL_BLEND)
        (glDisable GL_TEXTURE_2D)
        (glEnable GL_DEPTH_TEST)

        ;; glDeleteTextures(1, &texture);
        ;; TTF_CloseFont(font);
        ;; SDL_FreeSurface(sFont);

        ))

    ;; glBegin(GL_QUADS);
    ;; {
    ;;   glTexCoord2f(0,0); glVertex2f(x, y);
    ;;   glTexCoord2f(1,0); glVertex2f(x + sFont->w, y);
    ;;   glTexCoord2f(1,1); glVertex2f(x + sFont->w, y + sFont->h);
    ;;   glTexCoord2f(0,1); glVertex2f(x, y + sFont->h);
    ;; }
    ;; glEnd();

    ;; glMatrixMode(GL_PROJECTION);
    ;; glPopMatrix();
    ;; glMatrixMode(GL_PROJECTION);
    ;; glPopMatrix();


    ))

(define example-graph
  '(root:graph
    (group:
     (info: (name: "my group"))
     (elements:
      (polyline: (1 1) (0 0) (2 2) (3 3))
      (polyline: (3 2) (1 4) (5 2) (2 3))
      (polyline: (2 1) (4 4) (2 4) (0 3))))
    (text:
     (font: "assailand")
     (size: 14)
     (string: "Hello world!")
     (position: 0 0))))

(define (renderer:render)
  (glClearColor 0.0 0.0 0.0 1.0)
  (glClear GL_COLOR_BUFFER_BIT)

  ;; Create VBOs of dirty vertex objects
  (for-each
   (lambda (vo) (when (vertex-object-dirty vo)
             (vertex-object-vbo-set! vo (f32vector->gl-buffer (vertex-object-vertices vo) GL_STREAM_DRAW))
             (vertex-object-dirty-set! vo #f)))
   *vertex-objects*)

  ;; Render lines
  (with-program
   (table-ref *programs* 'lines)
   (lambda (program-id)
     (for-each
      (lambda (vo) (gl-draw-with-vbo (vertex-object-vbo vo)
                                GL_LINE_STRIP
                                (f32vector-length (vertex-object-vertices vo))
                                (lambda ()
                                  (let ((pos (glGetAttribLocation program-id "position")))
                                    (glEnableVertexAttribArray pos)
                                    (glVertexAttribPointer pos 2 GL_FLOAT GL_FALSE 0 (integer->void* 0))))))
      *vertex-objects*)))

  ;; Render Texture 2D
  ;; TODO
  (with-program
   (table-ref *programs* 'tex2d)
   (lambda (_) '_))
  )
