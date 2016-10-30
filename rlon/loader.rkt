; --------------------
; Rlon Loader
; --------------------

; --------------------
; File Langauge
; --------------------
#lang racket

; --------------------
; Requirements
; --------------------
(require
  rlon/expressions
  rlon/parser
  rlon/env
)

; --------------------
; Provisions
; --------------------
(provide
  (all-defined-out)
)

; --------------------
; Util
; --------------------
(define (rlon-load-port port [fmap (env)])
  (rlon-load (rlon-parse-port port) fmap)
)

(define (rlon-load-string input [fmap (env)])
  (rlon-load (rlon-parse-string input) fmap)
)

(define (rlon-load-file filename [fmap (env)])
  (rlon-load (rlon-parse-file filename) fmap)
)

; --------------------
; Methods
; --------------------
(define (rlon-load expr [fmap (env)])
  (match expr
    ((import-expr name rest)
      (printf "Importing: '~a'\n" name)
      (rlon-load rest (rlon-load (rlon-parse-file name)))
    )
    ((func-expr ident func rest)
      (rlon-load rest (extend-env fmap ident func))
    )
    ('EOF
      fmap
    )
  )
)
