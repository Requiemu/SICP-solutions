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

(define (stream-add a b)
  (stream-cons (+ (stream-first a) (stream-first b))
               (stream-add (stream-rest a) (stream-rest b))))

(define (stream-div s1 s2)
  (stream-cons (/ (stream-first s1)
                  (stream-first s2))
               (stream-div (stream-rest s1)
                           (stream-rest s2))))

(define (stream-mul s1 s2)
  (stream-cons (* (stream-first s1)
                  (stream-first s2))
               (stream-mul (stream-rest s1)
                           (stream-rest s2))))

(define (stream-scale stream factor)
  (stream-map (lambda (x) (* x factor)) stream))


(define (RC r c dt)
  (define (cal v0 i)
    (define result
      (stream-cons 
       (+ v0
          (/ (stream-first i) c)
          (* (stream-first i) r))
       (stream-map
        (lambda (x) (+ x v0))
        (stream-add 
         (stream-add result (stream-rest i))
         (stream-scale i r)))))
    result)
  cal)

(define RC1 (RC 5 1 0.5))

(define experiment (RC1 3 ones))

(display (stream-ref experiment 0))
(newline)

(display (stream-ref experiment 1))
(newline)

(display (stream-ref experiment 2))
(newline)

(display (stream-ref experiment 3))
(newline)

(display (stream-ref experiment 4))
(newline)

(display (stream-ref experiment 5))
(newline)

(display (stream-ref experiment 6))
(newline)

(display (stream-ref experiment 7))
(newline)

(display (stream-ref experiment 8))
(newline)

(display (stream-ref experiment 9))
(newline)

(display (stream-ref experiment 5000))
