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

(define test (pairs integers integers))

;;for (x,y), its order is :
;;when y=x, the order is (2^x)-2;
;;when y-x=1, the order is (2^x)+(2^(x-1))-2
;;when y-x>1, the order is (y-x+1)(2^x)+(2^(x-1))-2

(define (num x y) 
  (- (+ (* (- y x) (expt 2 x)) (expt 2 (- x 1))) 2))

(display (stream-ref test 197))
(newline)

;(display (stream-ref test (num 99 100)));;actually this is too big to run on my computer.
;(newline)
;
;(display (stream-ref test (num 100 100)))
;(newline) 

(display (stream-ref test (num 7 8)))
