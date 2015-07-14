#lang racket
(define (f x)
  (letrec ((even?
            (lambda (n)
              (if (= n 0)
                  true
                  (odd? (- n 1)))))
           (odd?
            (lambda (n)
              (if (= n 0)
                  false
                  (even? (- n 1))))))
    (odd? 3)))

(define (g x)
  (letrec ((x (lambda (k) (y k))) (y (lambda (x) x))) x))

(g 3)

(define (k x)
  (letrec ((j (lambda (i) (+ u i))) (u 3))
    (j x)))

(k 3)

