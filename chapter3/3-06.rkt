#lang racket
;(random-seed 2)
;(define g (make-pseudo-random-generator))
;(define k (current-pseudo-random-generator))
;(define e (current-pseudo-random-generator))
;(random-seed 2)
;(random e)
;(random-seed 2)
;(random k)
;(random g)
(define (make-rand-update)
  (let ((g (current-pseudo-random-generator)))
    (lambda (y)
      (if (inexact? y)
          (begin (random-seed (round (inexact->exact (* y 10000))))
                 (random g))
          (begin (random-seed y)
                 (random g))))))

(define rand-update (make-rand-update))

(rand-update 2)

(rand-update 2)

(rand-update (rand-update 2))

(rand-update (rand-update (rand-update 2)))

(rand-update (rand-update 2))
(define rand0
  (let ((x 2))
    (lambda ()
      (set! x (rand-update x))
      x)))


(define rand
  (let ((x 2))
    (lambda (s)
      (cond ((eq? s 'generate) 
             (begin (set! x (rand-update x)) x))
            ((eq? s 'reset)
             (lambda (i)
               (set! x i)))
            (else 'unknown-request)))))

(display "-----------------")
(newline)

(rand 'generate)

(rand 'generate)

((rand 'reset) 2)

(rand 'generate)






