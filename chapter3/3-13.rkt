#lang planet neil/sicp

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;(last-pair z) It will cause infinite cycling.

(define (change x) (set! x 'a))

(define (f t)
  (begin (change t)
         t))

(f 'b)
