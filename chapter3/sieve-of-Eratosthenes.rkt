#lang racket

(require racket/stream)

;(define (memo-proc proc)
;  (let ((already-run? false) (result false))
;    (lambda ()
;      (if (not already-run?)
;          (begin (set! result (proc))
;                 (set! already-run? true)
;                 result)
;          result))))
;
;(define (delay f) (memo-proc (lambda () f)))
;
;(define (force f) (f))
;
;;;delay and force implementation
;
;(define the-empty-stream '())
;
;(define (stream-null? x)
;  (null? x))
;
;(define (cons-stream a b)
;  (cons a (delay b)))
;
;(define (stream-car s) (car s))
;
;(define (stream-cdr s) (force (cdr s)))

;;basic stream construction and fetch defination

;(define (stream-ref s n)
;  (if (= n 0)
;      (stream-car s)
;      (stream-ref (stream-cdr s) (- n 1))))
;
;(define (stream-map proc . argstreams)
;  (if (null? (stream-car argstreams))
;      the-empty-stream
;      (cons-stream
;       (apply proc (map stream-car argstreams))
;       (apply stream-map
;              (cons proc (map stream-cdr argstreams))))))
;
;(define (stream-for-each proc s)
;  (if (stream-null? s) 
;      the-empty-stream
;      (begin (proc (stream-car s))
;             (stream-for-each proc (stream-cdr s)))))
;
;(define (display-stream s)
;  (stream-for-each display-line s))
;
;(define (display-line x)
;  (newline)
;  (display x))
;
;(define (stream-filter pred stream)
;  (cond ((stream-null? stream) the-empty-stream)
;        ((pred (stream-car stream))
;         (cons-stream (stream-car stream)
;                      (stream-filter pred
;                                     (stream-cdr stream))))
;        (else (stream-filter pred (stream-cdr stream)))))
;
;;;stream operation 
;
;(define (stream-enumerate-interval low high)
;  (if (> low high)
;      the-empty-stream
;      (cons-stream
;       low
;       (stream-enumerate-interval (+ low 1) high))))
;
;(define (show x)
;  (display-line x)
;  x)

;;stream application

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y)
  (= 0 (remainder x y)))

(define (sieve stream)
  (stream-cons
   (stream-first stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-first stream))))
                              (stream-rest stream)))))

(define primes (sieve (integers-starting-from 2)))

(stream-ref primes 50)