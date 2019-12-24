(define-library (render)

  (import (gambit))
  (import (_match))
  (import (github.com/alvatar/base))
  (import (github.com/alvatar/gles2))
  (import (github.com/alvatar/ffi-utils))
  (import (github.com/alvatar/sdl2))
  (import (github.com/alvatar/sdl2 ttf))
  (import (github.com/alvatar/math matrix))
  (import (github.com/alvatar/math vector2))

  (export renderer:init
          renderer:shutdown
          renderer:render)

  (begin
    (include "globals.scm")
    (include "gl-util.scm")
    (include "color.scm")
    (include "box2d.scm")
    (include "vbo.scm")
    (include "program.scm")
    (include "texture.scm")
    (include "text.scm")
    (include "render.scm")))
