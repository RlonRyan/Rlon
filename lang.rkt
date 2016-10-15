; Rlon Evaluator
#lang racket

; Imports
(require
  racket/generator
  2htdp/batch-io
  "boolutil.rkt"
  "miscutil.rkt"
  "lexer.rkt"
  "parser.rkt"
)

; Provides
(provide (all-defined-out))

; Debug
;(define i 0)

; Methods
(define (rlon-eval-this input)
  ;(set! i 0)
  (port-count-lines! input)
  (car (rlon-eval (lang-map) (rlon-parse-this input) (cons (void) (void))))
)

(define (rlon-eval fmap expr arg)
  ;(set! i (+ i 1))
  ;(println (~a i "\t: " arg))
  (if (pair? arg) (void) (error (~a "Bad Stack!\n" arg)))
  (match expr
    ('ARG arg)
    ('VAR (pop arg))
    ((var-expr val in) (rlon-eval fmap in (set-val arg val)))
    ((func-expr ident func rest)
      (hash-set! fmap ident func)
      (rlon-eval fmap rest arg)
    )
    ((dot-expr from to)
      (rlon-eval fmap to (rlon-eval fmap from arg))
    )
    ((if-expr from then else)
      (if (car (rlon-eval fmap from arg))
        (rlon-eval fmap then arg)
        (rlon-eval fmap else arg)
      )
    )
    ((branch-expr branch)
      (rlon-eval fmap branch arg)
      arg
    )
    (else (set-arg arg (rlon-eval-inner fmap expr arg)))
  )
)

(define (rlon-eval-inner fmap expr arg)
  (match expr
    ((literal-expr value) value)
    ((call-expr ident) (apply-ident fmap ident arg))
    ((unary func val)
      (func
        (car (rlon-eval fmap val arg))
      )
    )
    ((oper func left right)
      (func
        (car (rlon-eval fmap left arg))
        (car (rlon-eval fmap right arg))
      )
    )
    ((map-expr from mapper)
      (devoid
        (stream-map
          (lambda (arr) (car (rlon-eval fmap mapper (set-arg arg arr))))
          (car (rlon-eval fmap from arg))
        )
      )
    )
    ((filter-expr from filter)
      (stream-filter
        (lambda (arr) (car (rlon-eval fmap filter (set-arg arg arr))))
        (car (rlon-eval fmap from arg))
      )
    )
    ((list-expr elements)
      (devoid
        (stream-map
          (lambda (val) (car (rlon-eval fmap val arg)))
          (~s elements)
        )
      )
    )
    ((collect-expr from using)
      (stream-fold
        (lambda (a b)
          (car (rlon-eval fmap using (set-arg arg (list a b))))
        )
        (void)
        (car (rlon-eval fmap from arg))
      )
    )
  )
)

(define (apply-ident fmap ident arg)
  (let [(f (hash-ref fmap ident))]
    (cond
      [(procedure? f) (f (car arg))]
      [else (car (rlon-eval fmap f (arg-only arg)))]
    )
  )
)

(define (lang-map)
  (make-hash
    (list
      (cons "true" (wrap #t))
      (cons "false" (wrap #f))
      (cons "void" void)
      (cons "print" print)
      (cons "println" println)
      (cons "newl" newl)
      (cons "error" error)
      (cons "number" number)
      (cons "random" random)
      (cons "pow" expt)
      (cons "add" addarg)
      (cons "char" integer->char)
      (cons "code" char->integer)
      (cons "chars" string->list)
      (cons "string" ~ae)
      (cons "list" collate)
      (cons "fopen" fopen)
      (cons "regex" regex)
      (cons "env" getenv)
    )
  )
)

(define (wrap e)
  (lambda (a) e)
)

(define (devoid s)
  (stream-filter void?! s)
)

(define (list-wrap e)
  (list e)
)

(define (print a)
  (printf "~a" a)
)

(define (println a)
  (printf "~a\n" a)
)

(define (readline a)
  (read-line (current-input-port) 'any)
)

(define (addarg arg)
  (add (first arg) (second arg))
)

(define (collate arg)
  (append (~l (first arg)) (~l (second arg)))
)

(define (number a)
  (string->number (~a a))
)

(define (fopen a)
  (~s (read-file a))
)

(define (regex a)
  (regexp-match (regexp (~i a 0)) (~i a 1))
)

(define (set-arg arg val)
  (cons val (cdr arg))
)

(define (set-val arg val)
  (cons (car arg) val)
)

(define (arg-only arg)
  (cons (car arg) (void))
)

(define (pop arg)
  (let [(a (car arg))]
    (if (pair? a) a (cons a (void)))
  )
)

(define (newl a)
  (newline)
)
