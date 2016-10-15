; Rlon Lexer
#lang racket

; Imports
(require
  parser-tools/lex
  (prefix-in : parser-tools/lex-sre)
)

; Provides
(provide
  val-tokens
  op-tokens
  (all-defined-out)
)

; Tokens
(define-tokens val-tokens (
    IDENT
    STRING
    CHAR
    NUM
  )
)

(define-empty-tokens op-tokens (
    ; Structure
    LPAREN
    RPAREN
    BRANCH
    BRIDGE
    ARG
    VAR
    DOT
    MAP
    IF
    ; Lists
    SEPARATOR
    LBRACKET
    RBRACKET
    ; Functions
    LBRACE
    RBRACE
    ; Math
    POW
    MUL
    DIV
    MOD
    ADD
    MIN
    ; Logic
    OR
    NOT
    AND
    LESS
    MORE
    EQUAL
    ; Misc
    EOL
    EOF
  )
)

; Lexer
(define rlon-lexer
  (lexer-src-pos
    ; Literals
    [(:+ numeric) (token-NUM lexeme)]
    [(:: #\" (complement (:: any-string #\" any-string)) #\") (token-STRING (dequote lexeme))]
    [(:: #\' any-char #\') (token-CHAR lexeme)]
    [(:+ alphabetic) (token-IDENT lexeme)]
    ; Structure
    ["?" (token-IF)]
    ["." (token-DOT)]
    [":" (token-MAP)]
    [";" (token-EOL)]
    ["$" (token-BRANCH)]
    ["~" (token-BRIDGE)]
    ["@" (token-ARG)]
    ["#" (token-VAR)]
    ["(" (token-LPAREN)]
    [")" (token-RPAREN)]
    ; Lists
    ["," (token-SEPARATOR)]
    ["[" (token-LBRACKET)]
    ["]" (token-RBRACKET)]
    ; Functions
    ["{" (token-LBRACE)]
    ["}" (token-RBRACE)]
    ; Math
    ["^" (token-POW)]
    ["*" (token-MUL)]
    ["/" (token-DIV)]
    ["%" (token-MOD)]
    ["+" (token-ADD)]
    ["-" (token-MIN)]
    ; Logic
    ["|" (token-OR)]
    ["!" (token-NOT)]
    ["&" (token-AND)]
    ["<" (token-LESS)]
    [">" (token-MORE)]
    ["=" (token-EQUAL)]
    ; Misc
    [(:: "/*" (complement (:: any-string "*/" any-string)) "*/") (position-token-token (rlon-lexer input-port))]
    [(:+ whitespace) (position-token-token (rlon-lexer input-port))]
    [(eof) (token-EOF)]
  )
)

; Util
(define (rlon-lex-this input)
  (lambda () (rlon-lexer input))
)

(define (dequote s)
  (string-trim s "\"")
)
