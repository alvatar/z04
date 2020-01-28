(define-library (core)

  (import (gambit)
          ;;(_match)
          ;;(srfi 28)
          ;;(srfi 69)
          )

  (import (base memoization))

  (export
   make-color
   color?
   color-r
   color-g
   color-b
   color-a
   color-r-set!
   color-g-set!
   color-b-set!
   color-a-set!
   uuid-v4
   (rename get-test-data core-graph:get-test-data)
   )

  (begin
    (include "color.scm")
    (include "layer.scm")
    (include "graph.scm")
    (include "utils.scm")
    (include "style.scm")))
