; --------------------
; Module Expander
; --------------------

; --------------------
; File Langauge
; --------------------
#lang br/quicklang

; --------------------
; Requirements
; --------------------
(require
  rlon/lang
  rlon/loader
  rlon/lang/reader
)

; --------------------
; Provisions
; --------------------
(provide
  #%top-interaction
  (rename-out [rlon-module-begin #%module-begin])
)

; --------------------
; Methods
; --------------------
(define (cpr-rlon)
  (let
    ([res
      (let ([in ((current-get-interaction-input-port))])
        ((current-read-interaction) (object-name in) in)
      )
    ])
    (printf "rlon> ~a" res)
    res
  )
)

(define-macro (rlon-module-begin PARSE-TREE)
  #'(
    #%module-begin
    (rlon-call "main" (rlon-load PARSE-TREE))
  )
)
