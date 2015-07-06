#lang planet neil/sicp
(define x '(a b))

(define z1 (cons x x))

(define z2 (cons '(a b) '(a b)))

(equal? z1 z2)

(eq? z1 z2)

;(set-car! x 'b)
;
;(display z1)
;(display z2)

(define (set-to-wow! x)
  (set-car! (car x) 'wow))

(set-to-wow! z1)
(set-to-wow! z2)

(display z1)
(display z2)