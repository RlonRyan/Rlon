; --------------------
; Module Reader
; --------------------

; --------------------
; Language Declaration
; --------------------
#lang br/quicklang

; --------------------
; Requirements
; --------------------
(require
  syntax/strip-context
  rlon/parser
)

; --------------------
; Provisions
; --------------------
(provide
  (rename-out
    [rlon-read read]
    [rlon-get-info get-info]
    [rlon-read-syntax read-syntax]
  )
)

(define (rlon-read in)
  (syntax->datum
    (rlon-read-syntax #f in)
  )
)

(define (rlon-read-syntax path port)
  (let*
    (
      [parse-tree (rlon-parse-port port)]
      [module-datum `(module rlon-mod rlon/lang/expander,parse-tree)]
    )
    (datum->syntax #f module-datum)
  )
)

(define (rlon-get-info in mod line col pos)
  (lambda (key default)
    (case key
      ;[(color-lexer) (dynamic-require 'rlon/lang/color-lexer 'rlon-color-lexer)]
      [(drracket:default-filters) (list '("Rlon" "*.rlon"))]
      [else default]
    )
  )
)
