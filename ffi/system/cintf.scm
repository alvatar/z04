(c-declare #<<c-declare-end

#include "bgfx/c99/bgfx.h"
#include "cintf.h"

c-declare-end
)

;; Custom BGFX functions

(define graphics:init
  (c-lambda ((pointer (struct "SDL_SysWMinfo")) int int) int
            "___return(system_window_init(___arg1, ___arg2, ___arg3));"))

;; BGFX bindings

(define graphics:frame (c-lambda (bool) void "bgfx_frame"))

(define graphics:shutdown (c-lambda () void "bgfx_shutdown"))
