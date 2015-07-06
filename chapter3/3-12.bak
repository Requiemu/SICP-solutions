#lang planet neil/sicp
(define (append! x y)
  (begin (set-cdr! (last-pair x) y)
         x))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define a (cons '1 '2))

;(set! (car a) 'x)

(define x1 'x1)
(define x2 'x2)

(define b (cons x1 x2))

(set! b 'x)

(define c (cons 'x (cons 'y 'z)))

(set-car! (cdr c) 'rrr)

(define x (list 'a 'b))

(define y (list 'c 'd))

(define z (append x y))

(display (cdr x))
(newline)

(define w (append! x y))

(display (cdr x))
(newline)

;(set! x (list 'k 'k))

(display z)
(newline)

(display w)
(newline)