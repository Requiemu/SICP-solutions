#lang planet neil/sicp
(define t '((1 2) (3 4)))

(define (g x) (set-car! x (+ (car x) 1)))

(define tt (for-each g t))

(display tt)
(newline)
(display t)
(newline)

(define ttt '(1 2 3))

(define (f x) (+ x 1))

(define tttt (map f ttt))

(display tttt)
(newline)
(display ttt)
(newline)

(define ts (map g t))

(display ttt)
(newline)

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(display (enumerate-interval 2 10))