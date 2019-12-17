(define-library (render)

  (import (gambit))

  (import (github.com/alvatar/gles2))
  (import (github.com/alvatar/ffi-utils))

  (export renderer:init
          renderer:shutdown
          renderer:render)

  (include "render.scm"))
