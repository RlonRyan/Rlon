; --------------------
; Rlon Interpreter
; --------------------

; --------------------
; File Langauge
; --------------------
#lang racket

; --------------------
; Requirements
; --------------------
(require
  rlon/lang
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
;(define verbose-mode (make-parameter #f))

(command-line
  #:program "rlon"
  ;#:once-each
  ;[("-v" "--verbose") "Compile with verbose messages" (verbose-mode #t)]
  #:args
  (filename) (rlon-eval-file filename)
)
