(define-type color
  r g b a)

(define (SDL_Color->color sdl-color)
  (make-color
   (SDL_Color-r sdl-color)
   (SDL_Color-g sdl-color)
   (SDL_Color-b sdl-color)
   (SDL_Color-a sdl-color)))

(define (color->SDL_Color color)
  (let ((sdl-color* (alloc-SDL_Color)))
    (SDL_Color-r-set! sdl-color* (color-r color))
    (SDL_Color-g-set! sdl-color* (color-g color))
    (SDL_Color-b-set! sdl-color* (color-b color))
    (SDL_Color-a-set! sdl-color* (color-a color))
    (*->SDL_Color sdl-color*)))
