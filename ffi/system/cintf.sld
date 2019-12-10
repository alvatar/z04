(define-library (ffi/system/cintf)

  (import (gambit))

  (export
   graphics:init
   graphics:frame
   graphics:shutdown)

  (include "cintf.scm"))
