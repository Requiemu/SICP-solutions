#lang racket

(require racket/stream)

(define (integers-from n)
  (stream-cons n (integers-from (+ n 1))))

(define integers (integers-from 1))

(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons (stream-first s1)
                   (interleave s2 (stream-rest s1)))))


(define (interleave-3 s1 s2 s3)
  (if (stream-empty? s1) (interleave s2 s3)
      (stream-cons (stream-first s1)
                   (interleave-3 s2 s3 s1))))

(define (pairs s t)
  (stream-cons (list (stream-first s) (stream-first t))
               (interleave-3
                (stream-map (lambda (x) (list (stream-first s) x)) (stream-rest t))
                (stream-map (lambda (x) (list x (stream-first t))) (stream-rest s))
                (pairs (stream-rest s) (stream-rest t)))))

(stream-ref (pairs integers integers) 2)
               
