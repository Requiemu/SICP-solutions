#lang racket

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((element-of-set? (car s1) s2)
         (union-set (cdr s1) s2))
        ((not (element-of-set? (car s1) s2))
         (cons (car s1) (union-set (cdr s1) s2)))))

(union-set '(2 1 3 5) '(5 3 4 6))

