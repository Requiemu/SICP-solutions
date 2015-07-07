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

(define (stream-sub a b)
  (stream-cons (- (stream-first a) (stream-first b))
               (stream-sub (stream-rest a) (stream-rest b))))

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


;(define (integral integrand initial-value dt)
;  (define int
;    (cons-stream initial-value
;                 (stream-add (stream-scale integrand dt)
;                             int)))
;  int)

(define (integral delayed-integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (let ((integrand (force delayed-integrand)))
                   (stream-add (stream-scale integrand dt)
                               int))))
  int)

;(define (solve f y0 dt)
;  (define y (integral (delay dy) y0 dt))
;  (define dy (stream-map f y))
;  y)
;
;;(define (solve-2nd a b dt y0 dy0)
;;  (define y (integral (delay dy) y0 dt))
;;  (define dy (integral (delay ddy) dy0 dt))
;;  (define ddy (stream-add (stream-scale dy a)
;;                          (stream-scale y b)))
;;  y)
;
;(define (solve-2nd f y0 dy0 dt)
;  (define y (integral (delay dy) y0 dt))
;  (define dy (integral (delay ddy) dy0 dt))
;  (define ddy (f dy y))
;  y)
;
;(stream-ref (solve (lambda (y) y) 1 0.001) 1000)
;;
;;(stream-ref (solve (lambda (y) (* 2 y)) 1 0.001) 1000)
;
;(stream-ref (solve-2nd 
;             (lambda (x y) (stream-sub (stream-scale x 2) y))
;             1 
;             1 
;             0.001) 
;            1000)

(define (make-pair x y)
  (cond ((null? x) y)
        ((null? y) x)
        (else (stream-cons (cons (stream-first x) (stream-first y))
                           (make-pair (stream-rest x) (stream-rest y))))))

(define (RLC r l c dt)
  (define (circuit vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (stream-scale il (/ -1 c)))
    (define dil (stream-add (stream-scale il (/ (- r) l))
                            (stream-scale vc (/ 1 l))))
    (make-pair vc il))
  circuit)

(define the-circuit (RLC 1 0.2 1 0.1))

(define i-v-pairs (the-circuit 10 0))

(display (stream-ref i-v-pairs 0))
(newline)

(display (stream-ref i-v-pairs 1))
(newline)

(display (stream-ref i-v-pairs 2))
(newline)

(display (stream-ref i-v-pairs 3))
(newline)

(display (stream-ref i-v-pairs 4))
(newline)

(display (stream-ref i-v-pairs 5))
(newline)

(display (stream-ref i-v-pairs 6))
(newline)

(display (stream-ref i-v-pairs 7))
(newline)

(display (stream-ref i-v-pairs 8))
(newline)

(display (stream-ref i-v-pairs 9))
(newline)

(display (stream-ref i-v-pairs 5000))
