#lang racket

(define (fac n)
  (if (= n 1) 1
      (* n (fac (- n 1)))))

(fac 50)





;;for the test procedure here:
(define (fib n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(fib 24)

;;eval-apply mode: 5.2s

;;analyze-execute mode: 2,8s

;;the time for analyze is almost the same scale of the time of the execution.
