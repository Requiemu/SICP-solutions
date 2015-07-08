#lang racket
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? (car set) x) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set s1 s2)
  (append s1 s2))

(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((element-of-set? (car s1) s2)
         (cons (car s1) (intersection-set (cdr s1) s2)))
        (else (intersection-set (cdr s1) s2))))

(define t '(1 2 3 4 5 6 7 8 9))

(element-of-set? 3 t) 
(newline)

(element-of-set? 11 t)
(newline)

(adjoin-set 10 t)
(newline)

(adjoin-set 1 t)
(newline)

(define t0 '(a b c d e f g 1 2 3))

(union-set t t0)
(newline)

(intersection-set t t0)
(newline)

         