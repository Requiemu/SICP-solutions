#lang racket

(require racket/stream)

(define (stream-add a b)
  (stream-cons (+ (stream-first a) (stream-first b))
               (stream-add (stream-rest a) (stream-rest b))))

(define (ln2-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define (partial-sums stream)
  (define result (stream-cons (stream-first stream)
                              (stream-add (stream-rest stream) result)))
  result)

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(define (stream-limit stream tolerance)
  (define (iter a stream n)
    (if (< (abs (- a (stream-first stream))) tolerance)
        (cons (stream-first stream) n)
        (iter (stream-first stream) (stream-rest stream) (+ n 1))))
  (iter (stream-first stream) (stream-rest stream) 1))

(display (stream-limit ln2-stream 0.001))
(newline)

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-rest s)))))

(define (make-tableau transform s)
  (stream-cons s
               (make-tableau transform
                             (transform s))))

;(make-tableau euler-transform ln2-stream)

(define (accelerated-sequence transform s)
  (stream-map stream-first (make-tableau transform s)))

(define ac-ln2-stream (accelerated-sequence euler-transform ln2-stream))

(display (stream-limit ac-ln2-stream 0.001))
            




                                         
                                         
                                         