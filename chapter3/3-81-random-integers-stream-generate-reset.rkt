#lang racket

(require racket/stream)

(require r5rs)

(define rand-integer
  (let ((max (- (expt 2 32) 500)))
    (let ((x (random max)))
      (lambda (generator)
        (set! x (random max generator))
        x))))

(define (random-integers generator)
  (define result (stream-cons (rand-integer generator)
                              (stream-map (lambda (x) (rand-integer generator)) result)))
  result)

(define g (make-pseudo-random-generator))

(define r-s (random-integers g))

(define (make-random-stream-object stream)
  (let ((s stream) (n 0))
    (define (generate) 
      (begin (set! s (stream-rest s))
             (stream-first s)))
    (define (reset k)
      (begin (set! s (random-integers
                      (vector->pseudo-random-generator 
                       (vector k 2 3 4 5 6))))))
    (define (dispatch m)
      (cond ((eq? m 'generate) (generate))
            ((eq? m 'reset) reset)
            (else (error "MAKE-RANDOM-STREAM-OBJECT"))))
    dispatch))

(define r (make-random-stream-object r-s))
          

(display (r 'generate))
(newline)

((r 'reset) 5)

(display (r 'generate))


