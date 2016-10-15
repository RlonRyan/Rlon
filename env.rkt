; Rlon Evaluator
#lang racket

; Imports
; None

; Provides
(provide (all-defined-out))

; Methods
(define (empty-env)
  (lambda (searchVar)
    (error "No binding found!(")
  )
)

(define (extend-env env name val)
  (lambda (get)
    (if (equal? get name)
      val
      (apply-env env get)
    )
  )
)

(define (apply-env env find)
  (env find)
)
