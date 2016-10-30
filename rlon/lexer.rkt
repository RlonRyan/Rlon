; --------------------
; Rlon Lexer
; --------------------

; --------------------
; File Langauge
; --------------------
#lang racket

; --------------------
; Utility
; --------------------
(require
  parser-tools/lex
  (prefix-in : parser-tools/lex-sre)
)

; --------------------
; Provisions
; --------------------
(provide
  (all-defined-out)
)

; --------------------
; Tokens
; --------------------
(define-tokens val-tokens (
    IDENT
    STRING
    VOID
    CHAR
    BOOL
    NUM
  )
)

(define-empty-tokens op-tokens (
    ; Import
    IMPORT

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

; --------------------
; Lexer Abbr.
; --------------------
(define-lex-abbrevs
  (id (:: alphabetic (:* (:or alphabetic numeric "_"))))
  (char (:: #\' any-char #\'))
  (string (:: #\" (complement (:: any-string #\" any-string)) #\"))
  (integer (:+ numeric))
  (number (:or integer (:: integer "." integer)))
  (comment (:: "/*" (complement (:: any-string "*/" any-string)) "*/"))
  (true (:or "(true)" "(True)"))
  (false (:or "(false)" "(False)"))
  (:void (:or "()" "(void)" "(Void)"))
  (space (:+ whitespace))
)

; --------------------
; Lexer Definition
; --------------------
(define rlon-lexer
  (lexer-src-pos
    ; Misc
    ["import" (token-IMPORT)]
    ["#lang rlon" (return-without-pos (rlon-lexer input-port))]
    [comment (return-without-pos (rlon-lexer input-port))]
    [space (return-without-pos (rlon-lexer input-port))]
    [(eof) (token-EOF)]
    ; Literals
    [number (token-NUM (string->number lexeme))]
    [string (token-STRING (dequote lexeme))]
    [char (token-CHAR (string-ref lexeme 1))]
    [id (token-IDENT lexeme)]
    [false (token-BOOL #f)]
    [true (token-BOOL #t)]
    [:void (token-VOID (void))]
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
  )
)

; --------------------
; Utility
; --------------------
(define (rlon-lex-port port)
  (port-count-lines! port)
  (lambda () (rlon-lexer port))
)

(define (rlon-lex-string input)
  (let [(in (open-input-string input))]
    (port-count-lines! in)
    (lambda () (rlon-lexer in))
  )
)

(define (rlon-lex-file filename)
  (let [(in (open-input-file filename))]
    (port-count-lines! in)
    (lambda () (rlon-lexer in))
  )
)

(define (dequote s)
  (string-trim s "\"")
)
