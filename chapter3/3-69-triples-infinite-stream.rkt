#lang racket

(require racket/stream)

(define (integers-from n)
  (stream-cons n (integers-from (+ n 1))))

(define integers (integers-from 1))

(define (pairs s t)
  (stream-cons 
   (list (stream-first s) (stream-first t))
   (interleave
    (stream-map (lambda (x) (list (stream-first s) x))
                (stream-rest t))
    (pairs (stream-rest s) (stream-rest t)))))

(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons (stream-first s1)
                   (interleave s2 (stream-rest s1)))))

(define (triples s1 s2 s3)
  (stream-cons (list (stream-first s1)
                     (stream-first s2)
                     (stream-first s3))
               (interleave 
                (stream-map (lambda (x) (cons (stream-first s1) x)) 
                            (pairs s2 (stream-rest s3)))
                (triples (stream-rest s1)
                         (stream-rest s2)
                         (stream-rest s3)))))

(define test (triples integers integers integers))

(display (stream-ref test 0))
(newline)

(display (stream-ref test 1))
(newline)

(display (stream-ref test 2))
(newline)

(display (stream-ref test 3))
(newline)

(display (stream-ref test 4))
(newline)

(display (stream-ref test 5))
(newline)

(display (stream-ref test 6))
(newline)

(display (stream-ref test 7))
(newline)

(display (stream-ref test 8))
(newline)

(display (stream-ref test 9))
(newline)



                         