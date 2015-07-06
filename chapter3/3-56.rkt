#lang racket
(require racket/stream)

(define (stream-scale stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (merge s1 s2)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let ((s1car (stream-first s1))
               (s2car (stream-first s2)))
           (cond ((< s1car s2car)
                  (stream-cons s1car (merge (stream-rest s1) s2)))
                 ((> s1car s2car)
                  (stream-cons s2car (merge s1 (stream-rest s2))))
                 (else
                  (stream-cons s1car
                               (merge (stream-rest s1)
                                      (stream-rest s2)))))))))

(define S (stream-cons 1 (merge (stream-scale S 2) 
                                (merge (stream-scale S 3)
                                       (stream-scale S 5)))))

(display (stream-ref S 1))
(newline)

(display (stream-ref S 2))
(newline)

(display (stream-ref S 3))
(newline)

(display (stream-ref S 4))
(newline)

(display (stream-ref S 5))
(newline)

(display (stream-ref S 6))
(newline)