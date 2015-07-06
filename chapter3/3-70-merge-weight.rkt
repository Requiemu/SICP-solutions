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

(define (merge-weighted s1 s2 weight)
  (let ((s1-car (stream-first s1)) (s2-car (stream-first s2)))
    (cond ((> (weight s1-car) (weight s2-car)) 
           (stream-cons s2-car (merge-weighted s1 (stream-rest s2) weight)))
          ((< (weight s1-car) (weight s2-car))
           (stream-cons s1-car (merge-weighted (stream-rest s1) s2 weight)))
          ((= (weight s1-car) (weight s2-car))
           (if (equal? s1-car s2-car) 
               (stream-cons s1-car (merge-weighted (stream-rest s1)
                                                   (stream-rest s2)
                                                   weight))
               (stream-cons s1-car (stream-cons s2-car 
                                                (merge-weighted (stream-rest s1)
                                                                (stream-rest s2)
                                                                weight)))))
          (else (error "MERGE-WEIGHTED type dispatch" s1-car s2-car)))))








(define (weight-pairs s t weight)
  (stream-cons 
   (list (stream-first s) (stream-first t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-first s) x))
                (stream-rest t))
    (pairs (stream-rest s) (stream-rest t))
    weight)))

(define test (weight-pairs integers integers (lambda (x) (+ (car x) (cadr x)))))

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

(display "-----------------------")
(newline)

(define (weight235 x) (+ (* 2 (car x)) (* 3 (cadr x)) (* 5 (car x) (cadr x))))

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

(define test235 (weight-pairs S S weight235))

(display (stream-ref test235 0))
(weight235 (stream-ref test235 0))
(newline)

(display (stream-ref test235 1))
(weight235 (stream-ref test235 1))
(newline)

(display (stream-ref test235 2))
(weight235 (stream-ref test235 2))
(newline)

(display (stream-ref test235 3))
(weight235 (stream-ref test235 3))
(newline)

(display (stream-ref test235 4))
(weight235 (stream-ref test235 4))
(newline)

(display (stream-ref test235 5))
(weight235 (stream-ref test235 5))
(newline)

(display (stream-ref test235 6))
(weight235 (stream-ref test235 6))
(newline)

(display (stream-ref test235 7))
(weight235 (stream-ref test235 7))
(newline)

(display (stream-ref test235 8))
(weight235 (stream-ref test235 8))
(newline)

(display (stream-ref test235 9))
(weight235 (stream-ref test235 9))
(newline)

                                                 
                                                  
                                                  
                                                  