(require test-engine/racket-tests)
(require 2htdp/batch-io)
(require "rlon/lang.rkt")

(define (append-file name string)
  (write-file name (~a (read-file name) "\n" string))
)

(define (save-func name pred args function)
  (write-file
    (~a "racket\\" name ".rkt")
    (~a "(define (" name pred " " args ")\n" function "\n)\n")
  )
)

(define (load-func name)
  (load (~a "racket\\" name ".rkt"))
)

(define (rlon file)
  (rlon-eval-this (open-input-file (~a "racket\\" file ".rlon")))
)

(define (rlon-cli in)
  (rlon-eval-this (open-input-string in))
)

(define (~ae e)
  (cond
    [(not (list? e)) (~a e)]
    [(not (empty? e)) (~a (car e) (~ae (cdr e)))]
    [else ""]
  )
)
