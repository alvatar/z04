(c-declare #<<c-declare-end

#include "cintf.h"

c-declare-end
)

(c-define-type SDL_SysWMinfo* (pointer (struct "SDL_SysWMinfo")))

(define bgfx-init
  (c-lambda (SDL_SysWMinfo* int int) int
            "___return(bgfx_init(___arg1, ___arg2, ___arg3));"))
