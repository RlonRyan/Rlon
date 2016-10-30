; Rlon Evaluator
#lang racket

; Imports
(require
  rlon/expressions
  rlon/miscutil
  rlon/loader
  rlon/env
)

; Provides
(provide
  rlon-eval-string
  rlon-eval-file
  rlon-eval-port
  rlon-eval
  rlon-call
)

; Debug
;(define i 0)

; --------------------
; Methods
; --------------------
(define (rlon-eval-port port)
  ;(set! i 0)
  (rlon-call "main" (rlon-load-port port))
)

(define (rlon-eval-string input)
  ;(set! i 0)
  (rlon-call "main" (rlon-load-string input))
)

(define (rlon-eval-file filename)
  ;(set! i 0)
  (rlon-call "main" (rlon-load-file filename))
)

(define (rlon-call name fmap)
  (apply-ident name fmap (cons (void) (void)))
)

; --------------------
; Evaluator Definition
; --------------------
(define (rlon-eval expr [fmap (env)] [arg (cons (void) (void))])
  ;(set! i (+ i 1))
  ;(println (~a i "\t: " arg))
  (if (pair? arg) (void) (error (~a "Bad Stack!\n" arg)))
  (match expr
    ('ARG arg)
    ('VAR (pop arg))
    ((var-expr val in) (rlon-eval in fmap (set-val arg (rlon-eval val fmap arg))))
    ((dot-expr from to)
      (rlon-eval to fmap (rlon-eval from fmap arg))
    )
    ((if-expr from then else)
      (if (car (rlon-eval from fmap arg))
        (rlon-eval then fmap arg)
        (rlon-eval else fmap arg)
      )
    )
    ((branch-expr branch)
      (rlon-eval branch fmap arg)
      arg
    )
    (else (set-arg arg (rlon-eval-inner expr fmap arg)))
  )
)

(define (rlon-eval-inner expr fmap arg)
  (match expr
    ((literal-expr value) value)
    ((call-expr ident) (apply-ident ident fmap arg))
    ((unary func val)
      (func
        (car (rlon-eval val fmap arg))
      )
    )
    ((binary func left right)
      (func
        (car (rlon-eval left fmap arg))
        (car (rlon-eval right fmap arg))
      )
    )
    ((map-expr from mapper)
      (devoid
        (stream-map
          (lambda (arr) (car (rlon-eval mapper fmap (set-arg arg arr))))
          (car (rlon-eval from fmap arg))
        )
      )
    )
    ((filter-expr from filter)
      (stream-filter
        (lambda (arr) (car (rlon-eval filter fmap (set-arg arg arr))))
        (car (rlon-eval from fmap arg))
      )
    )
    ((list-expr elements)
      (devoid
        (stream-map
          (lambda (val) (car (rlon-eval val fmap arg)))
          (~s elements)
        )
      )
    )
    ((collect-expr from using)
      (stream-fold
        (lambda (a b)
          (car (rlon-eval using fmap (set-arg arg (list a b))))
        )
        (void)
        (car (rlon-eval from fmap arg))
      )
    )
  )
)

(define (apply-ident ident fmap arg)
  (let [(f (apply-env fmap ident))]
    (cond
      [(procedure? f) (f (car arg))]
      [else (car (rlon-eval f fmap (arg-only arg)))]
    )
  )
)

; --------------------
; Utility
; --------------------
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
  (let [(a (cdr arg))]
    (if (pair? a) a (cons a (void)))
  )
)
