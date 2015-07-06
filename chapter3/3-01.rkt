#lang planet neil/sicp

(define (make-accumulator initial)
  (define (accumulator x)
    (begin (set! initial (+ x initial))
           initial))
  accumulator)

(define a (make-accumulator 5))

(a 10)

(a 20)

(a 15)
