; Rlon Lexer
#lang racket

; Imports

; Provides
(provide
  (all-defined-out)
)

; Methods
(define (add a b)
  (cond
    [(equal? a (void)) b]
    [(equal? b (void)) a]
    [(and (number? a) (number? b)) (+ a b)]
    [else (~a a b)]
  )
)

(define (add-lists a b)
  (stream-append (~s a) (~s b))
)

(define (pair->list p)
  (append (vlist (car p)) (vlist (cdr p)))
)

(define (devoid s)
  (stream-filter (lambda (e) (not (equal? e (void)))) s)
)

(define (vlist a)
  (if (void? a) '() (list a))
)

(define (contains s a)
  (stream-ormap
    (lambda (e) (equal? a e))
    (~s s)
  )
)

(define (~len a)
  (stream-length (~s a))
)

(define (~l a)
  (cond
    [(void? a) '()]
    [(stream? a) (stream->list a)]
    [(pair? a) (pair->list a)]
    [(list? a) a]
    [else (list a)]
  )
)

(define (~s a)
  (cond
    [(stream? a) a]
    [(sequence? a) (sequence->stream a)]
    [else (in-list (list a))]
  )
)

(define (~rest a)
  (stream-rest (~s a))
)

(define (~i a i)
  (cond
    [(stream? a) (stream-ref a i)]
    [(list? a) (list-ref a i)]
    [(pair? a) (list-ref (~l a) i)]
    [(i = 0) (a)]
    [else (error (~a "Invalid Index!\n\t- Index: " i "\n\t- For:\n\t" a))]
  )
)

(define (~ae e)
  (stream-fold ~a "" (~s e))
)
