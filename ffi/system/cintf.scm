;; (##supply-module ffi/system/cintf)

(##namespace ("ffi/system/cintf#"))
(##include "~~lib/_prim#.scm")                   ;; map fx+ to ##fx+, etc
(##include "~~lib/_gambit#.scm")                 ;; for macro-check-procedure,
(##include "cintf#.scm")


(c-declare #<<c-declare-end

#include "cintf.h"

c-declare-end
)

(c-define-type SDL_SysWMinfo* (pointer (struct "SDL_SysWMinfo")))

(define bgfx-init
  (c-lambda (SDL_SysWMinfo* int int) int
            "___return(bgfx_init(___arg1, ___arg2, ___arg3));"))
