(define-library (render)

  (import (gambit)
          (_match)
          (srfi 28)
          (srfi 69)
          )

  (import (base)
          (base alist)
          (base functional)
          (base memoization)
          (gles2)
          (ffi-utils)
          (sdl2)
          (sdl2 ttf)
          (math matrix)
          (math vector2))

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
