#lang racket
(require racket/stream)

(define (mul-streams s1 s2)
  (stream-cons (* (stream-first s1) (stream-first s2))
               (mul-streams (stream-rest s1) (stream-rest s2))))

(define (integers-from n)
  (stream-cons n (integers-from (+ n 1))))

(define integers (integers-from 1))

(define factorials (stream-cons 1 (mul-streams factorials (stream-rest integers))))

(display (stream-ref factorials 5))

