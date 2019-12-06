(define-library (ffi/memory)

  (import (gambit))

  (export
   calloc
   malloc
   realloc
   free
   *-offset
   *->void*
   integer->void*
   *->string)

  (include "memory.scm"))
