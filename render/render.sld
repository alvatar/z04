(define-library (render)

  (import (gambit)
          (_match)
          (srfi 28)
          (srfi 69))

  (import (github.com/alvatar/base)
          (github.com/alvatar/base alist)
          (github.com/alvatar/base functional)
          (github.com/alvatar/base functional combinator)
          (github.com/alvatar/base memoization)
          (github.com/alvatar/gles2)
          (github.com/alvatar/ffi-utils)
          (github.com/alvatar/sdl2)
          (github.com/alvatar/sdl2 ttf)
          (github.com/alvatar/math matrix)
          (github.com/alvatar/math vector2))

  (import (core))

  (export renderer:init
          renderer:shutdown
          renderer:render
          renderer:load-scene!
          renderer:translate-view!
          renderer:scale-view!
          (rename fonts:install render-fonts:install)
          scene-tree.add-node!
          scene-tree.alter-node!
          scene-tree.remove-node!
          scene-tree.find-child
          scene-tree.filter-children

          ;; TMP
          make-node
          node-element
          node-type)

  (begin
    (include "box2d.scm")
    (include "gl-util.scm")
    (include "layer.scm")
    (include "polyline.scm")
    (include "program.scm")
    (include "render.scm")
    (include "text.scm")
    (include "texture.scm")
    (include "tree.scm")
    (include "vbo.scm")
    ))
