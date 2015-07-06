#lang racket
(require racket/stream)

(define (stream-display s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (integers-from n)
  (stream-cons n (integers-from (+ n 1))))

(define integers (integers-from 1))

(define ones (stream-cons 1 ones))

(define (stream-div s1 s2)
  (stream-cons (/ (stream-first s1)
                  (stream-first s2))
               (stream-div (stream-rest s1)
                           (stream-rest s2))))

(define (stream-scale stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (integrate-series s)
  (stream-div s integers))

(define integ-ones (integrate-series ones))

(display (stream-ref integ-ones 1))
(newline)

(display (stream-ref integ-ones 2))
(newline)

(display (stream-ref integ-ones 3))
(newline)

(display (stream-ref integ-ones 4))
(newline)

(display (stream-ref integ-ones 5))
(newline)

(display (stream-ref integ-ones 6))
(newline)

(display (stream-ref integ-ones 7))
(newline)

(display (stream-ref integ-ones 8))
(newline)

(display (stream-ref integ-ones 9))
(newline)

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))



(define cosine-series
  (stream-cons 1 (stream-scale (integrate-series sine-series) -1)))

(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

(display "sin")
(newline)

(display (stream-ref sine-series 0))
(newline)

(display (stream-ref sine-series 1))
(newline)

(display (stream-ref sine-series 2))
(newline)

(display (stream-ref sine-series 3))
(newline)

(display (stream-ref sine-series 4))
(newline)

(display (stream-ref sine-series 5))
(newline)

(display (stream-ref sine-series 6))
(newline)

(display (stream-ref sine-series 7))
(newline)

(display (stream-ref sine-series 8))
(newline)

(display (stream-ref sine-series 9))
(newline)



