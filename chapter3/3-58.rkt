#lang racket

(require racket/stream)

(define (stream-display s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (expand num den radix)
  (stream-cons
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(define 1-7-10 (expand 1 7 10))

;(stream-display 1-7-10)

(display (stream-ref 1-7-10 1))
(newline)

(display (stream-ref 1-7-10 2))
(newline)

(display (stream-ref 1-7-10 3))
(newline)

(display (stream-ref 1-7-10 4))
(newline)

(display (stream-ref 1-7-10 5))
(newline)

(display (stream-ref 1-7-10 6))
(newline)

(display (stream-ref 1-7-10 7))
(newline)

(display (stream-ref 1-7-10 8))
(newline)

(display (stream-ref 1-7-10 9))
(newline)

