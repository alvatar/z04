(define-type layer
  constructor: make-layer/internal
  default-style ; Default style for elements that have none defined
  elements ; Vector of elements in layer (references to nodes in scenegraph)
  )

(define (make-layer)
  ;; TODO: set default style
  (make-layer/internal (make-style) ))
