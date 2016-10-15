; Rlon Lexer
#lang racket

; Imports

; Provides
(provide
  (all-defined-out)
)

; Methods
(define (and? a b)
  (and a b)
)

(define (or? a b)
  (or a b)
)

(define (xor? a b)
  (and (not (and a b)) (or a b))
)

(define (neq? a b)
  (not (eq? a b))
)

(define (void? a)
  (eq? (void) a)
)

(define (void?! a)
  (not (void? a))
)
