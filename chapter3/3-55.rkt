#lang racket
(require racket/stream)

(define (integers-from n)
  (stream-cons n (integers-from (+ n 1))))

(define integers (integers-from 1))

(define (add-streams a b)
  (stream-cons (+ (stream-first a) (stream-first b))
               (add-streams (stream-rest a) (stream-rest b))))

(define (partial-sums s)
  (stream-cons (stream-first s)
               (add-streams s (partial-sums s))))

(define p-i (partial-sums integers))

(display (stream-ref p-i 5))