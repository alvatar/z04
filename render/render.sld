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
          renderer:load-scene!
          renderer:translate-view!
          renderer:scale-view!)

  (begin
    (include "box2d.scm")
    (include "color.scm")
    (include "gl-util.scm")
    (include "layer.scm")
    (include "polyline.scm")
    (include "program.scm")
    (include "render.scm")
    (include "scenegraph.scm")
    (include "text.scm")
    (include "texture.scm")
    (include "vbo.scm")
    ))
