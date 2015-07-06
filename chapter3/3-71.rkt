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
    (weight-pairs (stream-rest s) (stream-rest t) weight)
    weight)))


(define (cube x) (* x x x))

(define (weight-cube x) (+ (cube (car x)) (cube (cadr x))))

(define pairs-2b-check (weight-pairs integers integers weight-cube))
(define (equal-beside s weight)
  (if (= (weight (stream-first s)) (weight (stream-first (stream-rest s))))
      (stream-cons (list (stream-first s) 
                         (stream-first (stream-rest s)) 
                         (weight (stream-first s)))
                   (equal-beside (stream-rest s) weight))
      (equal-beside (stream-rest s) weight)))

(define Ramanujan-numbers (equal-beside pairs-2b-check weight-cube))


(display (stream-ref Ramanujan-numbers 0))
(newline)

(display (stream-ref Ramanujan-numbers 1))
(newline)

(display (stream-ref Ramanujan-numbers 2))
(newline)

(display (stream-ref Ramanujan-numbers 3))
(newline)

(display (stream-ref Ramanujan-numbers 4))
(newline)

(display (stream-ref Ramanujan-numbers 5))
(newline)

(display (stream-ref Ramanujan-numbers 6))
(newline)

(display (stream-ref Ramanujan-numbers 7))
(newline)

(display (stream-ref Ramanujan-numbers 8))
(newline)

(display (stream-ref Ramanujan-numbers 9))
(newline)

(display "-----------------------")
(newline)


;(stream-ref pairs-2b-check 1) (weight-cube (stream-ref pairs-2b-check 1))
;(newline)
;
;(stream-ref pairs-2b-check 2) (weight-cube (stream-ref pairs-2b-check 2))
;(newline)
;
;(stream-ref pairs-2b-check 3) (weight-cube (stream-ref pairs-2b-check 3))
;(newline)
;
;(stream-ref pairs-2b-check 4) (weight-cube (stream-ref pairs-2b-check 4))
;(newline)
;
;(stream-ref pairs-2b-check 5) (weight-cube (stream-ref pairs-2b-check 5))
;(newline)
;
;(stream-ref pairs-2b-check 6) (weight-cube (stream-ref pairs-2b-check 6))
;(newline)