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
      (cons "sqrt" sqrt)
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
      (cons "range" range)
      (cons "rest" ~rest)

      ; Misc
      (cons "number" number)
      (cons "char" integer->char)
      (cons "code" char->integer)
      (cons "chars" string->list)
      (cons "string" ~ae)
      (cons "list" ~l)
      (cons "not" not)
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
  (add-lists (~i a 0) (~i a 1))
)

(define (range a)
  (let ([l (~l a)])
    (in-range (~i l 0) (~i l 1))
  )
)

(define (number a)
  (string->number (~a a))
)

(define (fopen a)
  (~s (open-input-file a))
)

(define (regex a)
  (regexp-match (regexp (~i a 0)) (~i a 1))
)

(define (newl a)
  (newline)
)
