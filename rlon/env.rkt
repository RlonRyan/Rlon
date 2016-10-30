; --------------------
; Rlon Enviorment
; --------------------

; --------------------
; File Langauge
; --------------------
#lang racket

; --------------------
; Requirements
; --------------------
(require
  rlon/core
)

; --------------------
; Provisions
; --------------------
(provide
  (all-defined-out)
)

; --------------------
; Methods
; --------------------
(define (env [fmap (lang-map)])
  (lambda (ident)
    (cond
      [(hash-has-key? fmap ident) (hash-ref fmap ident)]
      [else (error (~a "No binding found for function: '" ident "'!"))]
    )
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
