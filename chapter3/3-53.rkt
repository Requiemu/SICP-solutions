#lang racket
(require racket/stream)

(define (add-stream a b)
  (stream-cons (+ (stream-first a) (stream-first b))
               (add-stream (stream-rest a) (stream-rest b))))

(define s (stream-cons 1 (add-stream s s)))

(display (stream-ref s 10))

;;the stream is the expotential of the 2.