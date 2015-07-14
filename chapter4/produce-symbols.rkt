#lang racket
(define (make-produce-symbol n)
    (define make 
      (lambda () 
        (set! n (+ n 1))
        (string->symbol (number->string n))))
    make)

(define sy (make-produce-symbol 1))

(sy)

(sy)

(map (lambda (x) (list (sy) x)) '(1 2 3 4))