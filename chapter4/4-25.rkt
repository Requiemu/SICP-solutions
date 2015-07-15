#lang racket
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

;(define (factorial n)
;  (unless (= n 1)
;    (* n (factorial (- n 1)))
;    1))

;;in the applicative situation this will run forever(the n will forever be minused by 1.

(define (factorial n)
  (if (not (= n 1))
      (* n (factorial (- n 1)))
      1))

(factorial 5)

;;This simulate the normal evaluation, and is can calculate the correct answer. 

