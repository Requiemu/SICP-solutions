#lang racket

(require racket/stream)

(define (sqrt-stream x)
  (define guesses
    (stream-cons 1.0 
                 (stream-map (lambda (guess) (sqrt-improve guess x)) guesses)))
  guesses)

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average x y) (/ (+ x y) 2))

(define test (sqrt-stream 4))

;(stream-ref test 18)

(define (stream-limit stream tolerance)
  (define (iter a stream)
    (if (< (abs (- a (stream-first stream))) tolerance)
        (stream-first stream)
        (iter (stream-first stream) (stream-rest stream))))
  (iter (stream-first stream) (stream-rest stream)))

(stream-limit test 0.1)

