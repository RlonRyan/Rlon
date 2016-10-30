; --------------------
; Rlon Expressions
; --------------------

; --------------------
; File Langauge
; --------------------
#lang racket

; --------------------
; Requirements
; --------------------
(require
  ; None
)

; --------------------
; Provisions
; --------------------
(provide
  (all-defined-out)
)

; --------------------
; Structs
; --------------------

; Imports
(struct import-expr (name rest) #:transparent)

; Literals
(struct literal-expr (value) #:transparent)

; Function Call
(struct call-expr (ident) #:transparent)

; Branch
(struct dot-expr (from then) #:transparent)
(struct map-expr (from mapper) #:transparent)
(struct filter-expr (from filter) #:transparent)
(struct collect-expr (from using) #:transparent)
(struct branch-expr (branch) #:transparent)
(struct if-expr (from then else) #:transparent)
(struct trans-expr (start end) #:transparent)

; Function
(struct func-expr (ident func rest) #:transparent)
(struct list-expr (elements) #:transparent)
(struct var-expr (val in) #:transparent)

; Operators (Functions)
(struct unary (func val) #:transparent)
(struct binary (func left right) #:transparent)
