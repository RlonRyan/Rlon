; --------------------
; Rlon Core Functions
; --------------------

; --------------------
; File Langauge
; --------------------
#lang racket

; --------------------
; Requirements
; --------------------
(require
  2htdp/batch-io
  math/base
  rlon/miscutil
  rlon/boolutil
)

; --------------------
; Provisions
; --------------------
(provide
  (all-defined-out)
)

(define (lang-map)
  (make-hash
    (list
      ; Constants
      (cons "true" (wrap #t))
      (cons "false" (wrap #f))
      (cons "void" void)

      ; Printing
      (cons "print" print)
      (cons "printf" rlon-printf)
      (cons "println" println)
      (cons "newl" newl)
      (cons "error" error)

      ; Math
      (cons "random" random)
      (cons "pow" expt)
      (cons "add" addarg)
      (cons "log" log)
      (cons "abs" abs)
      (cons "pi" (wrap pi))
      (cons "e" (wrap euler.0))
      (cons "sin" sin)
      (cons "cos" cos)
      (cons "tan" tan)
      (cons "asin" asin)
      (cons "acos" acos)
      (cons "atan" atan)
      (cons "radians" degrees->radians)
      (cons "degrees" radians->degrees)

      ; Lists
      (cons "join" join)

      ; Misc
      (cons "number" number)
      (cons "char" integer->char)
      (cons "code" char->integer)
      (cons "chars" string->list)
      (cons "string" ~ae)
      (cons "list" ~l)
      (cons "length" ~len)
      (cons "fopen" fopen)
      (cons "regex" regex)
      (cons "env" getenv)
    )
  )
)

(define (wrap e)
  (lambda (a) e)
)

(define (list-wrap e)
  (list e)
)

(define (print a)
  (printf "~a" a)
)

(define (rlon-printf a)
  (apply printf (~l a))
)

(define (println a)
  (printf "~a\n" a)
)

(define (readline a)
  (read-line (current-input-port) 'any)
)

(define (addarg a)
  (add (first a) (second a))
)

(define (join a)
  (append (~l (first a)) (~l (second a)))
)

(define (number a)
  (string->number (~a a))
)

(define (fopen a)
  (~s (read-file a))
)

(define (regex a)
  (regexp-match (regexp (~i a 0)) (~i a 1))
)

(define (newl a)
  (newline)
)
