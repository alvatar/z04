(define-library (render)

  (import (gambit))
  (import (_match))
  (import (srfi 28))
  (import (srfi 69))

  (import (github.com/alvatar/base))
  (import (github.com/alvatar/base alist))
  (import (github.com/alvatar/base functional))
  (import (github.com/alvatar/base functional combinator))
  (import (github.com/alvatar/base memoization))
  (import (github.com/alvatar/gles2))
  (import (github.com/alvatar/ffi-utils))
  (import (github.com/alvatar/sdl2))
  (import (github.com/alvatar/sdl2 ttf))
  (import (github.com/alvatar/math matrix))
  (import (github.com/alvatar/math vector2))

  (export renderer:init
          renderer:shutdown
          renderer:render
          renderer:set-test-data!
          renderer:translate-view!
          renderer:scale-view!)

  (begin
    (include "gl-util.scm")
    (include "program.scm")
    (include "color.scm")
    (include "box2d.scm")
    (include "vbo.scm")
    (include "texture.scm")
    (include "text.scm")
    (include "polyline.scm")
    (include "layer.scm")
    (include "render.scm")))
