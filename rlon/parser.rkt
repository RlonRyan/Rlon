; --------------------
; Rlon Parser
; --------------------

; --------------------
; File Langauge
; --------------------
#lang racket

; --------------------
; Requirements
; --------------------
(require
  parser-tools/lex
  parser-tools/yacc
  rlon/expressions
  rlon/boolutil
  rlon/miscutil
  rlon/lexer
)

; --------------------
; Provisions
; --------------------
(provide
  (all-defined-out)
)

; --------------------
; Parser Definition
; --------------------
(define rlon-parser
  (parser
    (src-pos)
    (start import)
    (end EOF)
    (tokens val-tokens op-tokens)
    (error
      (lambda (tok-ok? tok-name tok-value start-pos end-pos)
        (println (~a "Bad token: " tok-name " at: " (position-line start-pos) ":" (position-col start-pos)))
      )
    )
    ;(debug "rlon.debug")
    ;(yacc-output "rlon.lang")
    (grammar
      (import
        ((IMPORT qname EOL import) (import-expr $2 $4))
        ((def) $1)
      )
      (qname
        ((IDENT DOT qname) (~a $1 "\\" $3))
        ((IDENT) (~a $1 ".rlon"))
      )
      (def
        ; Function Definition
        ((LPAREN IDENT RPAREN MIN MORE LBRACE block RBRACE def) (func-expr $2 $7 $9))
        (() 'EOF)
      )
      (block
        ((VAR MIN MORE expr EOL block) (var-expr $4 $6))
        ((vals) (list-expr $1))
        ((expr) $1)
      )
      (vals
        ((expr SEPARATOR vals) (list* $1 $3))
        ((expr SEPARATOR expr) (list $1 $3))
      )
      (expr
        ((trans IF LBRACE block RBRACE) (if-expr $1 $4 'ARG))
        ((trans IF LBRACE block RBRACE BRIDGE expr) (if-expr $1 $4 $7))
        ((trans) $1)
      )
      (paren
        ((IDENT) (call-expr $1))
        ((expr EQUAL expr) (binary equal? $1 $3))
        ((expr NOT EQUAL expr) (binary nequal? $1 $4))
        ((expr LESS EQUAL expr) (binary <= $1 $4))
        ((expr MORE EQUAL expr) (binary >= $1 $4))
        ((expr LESS expr) (binary < $1 $3))
        ((expr MORE expr) (binary > $1 $3))
        ((expr AND expr) (binary and? $1 $3))
        ((expr OR expr) (binary or? $1 $3))
        ((expr MIN expr) (binary - $1 $3))
        ((expr DIV expr) (binary / $1 $3))
        ((expr MOD expr) (binary modulo $1 $3))
        ((expr POW expr) (binary expt $1 $3))
        ((add) $1)
        ((mul) $1)
      )
      (add
        ((expr ADD add) (binary add $1 $3))
        ((expr ADD expr) (binary add $1 $3))
      )
      (mul
        ((expr MUL mul) (binary * $1 $3))
        ((expr MUL expr) (binary * $1 $3))
      )
      (trans
        ((trans MAP MAP control) (collect-expr $1 $4))
        ((trans MAP IF control) (filter-expr $1 $4))
        ((trans MAP control) (map-expr $1 $3))
        ((trans DOT control) (dot-expr $1 $3))
        ((control) $1)
      )
      (control
        ((BRANCH brack) (branch-expr $2))
        ((brack) $1)
      )
      (brack
        ((brack LBRACKET IF value RBRACKET) (binary contains $1 $4))
        ((brack LBRACKET value RBRACKET) (binary ~i $1 $3))
        ((value) $1)
      )
      (value
        ; Literals
        ((ARG) 'ARG)
        ((VAR) 'VAR)
        ((NUM) (literal-expr $1))
        ((STRING) (literal-expr $1))
        ((CHAR) (literal-expr $1))
        ((BOOL) (literal-expr $1))
        ((VOID) (literal-expr $1))
        ; Parens
        ((LPAREN paren RPAREN) $2)
        ; Blocks
        ((LBRACE block RBRACE) $2)
      )
    )
  )
)

; --------------------
; Utility
; --------------------
(define (rlon-parse-port port)
  (rlon-parser (rlon-lex-port port))
)

(define (rlon-parse-string input)
  (rlon-parser (rlon-lex-string input))
)

(define (rlon-parse-file filename)
  (rlon-parser (rlon-lex-file filename))
)
