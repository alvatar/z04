(define-library (json)
  (import (gambit))

  (export
   json-decode
   json-encode
   json-read
   json-write
   json-error
   json-error?)

  (include "json.scm"))
