; Maths Parser
#lang racket

; Imports
(require
  parser-tools/lex
  parser-tools/yacc
  "boolutil.rkt"
  "miscutil.rkt"
  "lexer.rkt"
)

; Provides
(provide (all-defined-out))

; Structs

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
(struct oper (func left right) #:transparent)

; Parser
(define rlon-parser
  (parser
    (src-pos)
    (start def)
    (end EOF)
    (tokens val-tokens op-tokens)
    (error
      (lambda (tok-ok? tok-name tok-value start-pos end-pos)
        (println (~a "Bad token: " tok-name " at: " (position-line start-pos) ":" (position-col start-pos)))
      )
    )
    ;(debug "rlon.debug")
    (yacc-output "rlon.lang")
    (grammar
      (def
        ; Function Definition
        ((LPAREN IDENT RPAREN MIN MORE LBRACE block def) (func-expr $2 $7 $8))
        (() (call-expr "main"))
      )
      (block
        ((VAR MIN MORE expr EOL block) (var-expr $4 $6))
        ((addlist) $1)
        ((vals) (list-expr $1))
        ((expr EOL block) (dot-expr $1 $3))
        ((expr RBRACE) $1)
        ((RBRACE) 'ARG)
      )
      (vals
        ((expr SEPARATOR vals) (list* $1 $3))
        ((expr RBRACE) (list $1))
      )
      (addlist
        ((expr ADD addlist) (oper add-lists $1 $3))
        ((expr RBRACE) (list $1))
      )
      (expr
        ; Structure
        ((expr IF LBRACE block elif) (if-expr $1 $4 $5))
        ((trans) $1)
      )
      (elif
        ((BRIDGE trans IF LBRACE block elif) (if-expr $2 $5 $6))
        ((BRIDGE LBRACE block) $3)
        (() 'ARG)
      )
      (paren
        ((IDENT RPAREN) (call-expr $1))
        ((expr LESS MIN MORE expr RPAREN) (oper in-range $1 $5))
        ((expr EQUAL expr RPAREN) (oper eq? $1 $3))
        ((expr NOT EQUAL expr RPAREN) (oper neq? $1 $4))
        ((expr LESS EQUAL expr RPAREN) (oper <= $1 $4))
        ((expr MORE EQUAL expr RPAREN) (oper >= $1 $4))
        ((expr LESS expr RPAREN) (oper < $1 $3))
        ((expr MORE expr RPAREN) (oper > $1 $3))
        ((expr AND expr RPAREN) (oper and? $1 $3))
        ((expr OR expr RPAREN) (oper or? $1 $3))
        ((expr MIN expr RPAREN) (oper - $1 $3))
        ((expr DIV expr RPAREN) (oper / $1 $3))
        ((expr MOD expr RPAREN) (oper modulo $1 $3))
        ((add) $1)
        ((mul) $1)
      )
      (add
        ((expr ADD add) (oper add $1 $3))
        ((expr RPAREN) $1)
      )
      (mul
        ((expr MUL mul) (oper * $1 $3))
        ((expr RPAREN) $1)
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
        ((brack LBRACKET IF value RBRACKET) (oper contains $1 $4))
        ((brack LBRACKET BRIDGE value RBRACKET) (unary ~l $4))
        ((brack LBRACKET value RBRACKET) (oper ~i $1 $3))
        ((brack LBRACKET RBRACKET) (unary ~len $1))
        ((value) $1)
      )
      (value
        ; Literals
        ((ARG) 'ARG)
        ((VAR) 'VAR)
        ((NUM) (literal-expr (string->number $1)))
        ((STRING) (literal-expr (string-trim $1 "\"")))
        ((CHAR) (literal-expr (string-ref $1 1)))
        ; Parens
        ((LPAREN paren) $2)
        ; Blocks
        ((LBRACE block) $2)
        ; Callback
        ((expr) $1)
      )
    )
  )
)

; Util
(define (rlon-parse-this input)
  (rlon-parser (rlon-lex-this input))
)
