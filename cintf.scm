(c-declare #<<c-declare-end

#include "cintf.h"

c-declare-end
)

(define bgfx-init
  (c-lambda ((pointer void) int int) int
            "___return(bgfx_init(___arg1, ___arg2, ___arg3));"))
