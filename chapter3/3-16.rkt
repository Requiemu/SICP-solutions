#lang planet neil/sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define z3 '(a b c))

(count-pairs z3)

(define second (cons 'a 'b)) 
(define third (cons 'a 'b)) 
(define first (cons second third)) 
(set-car! third second) 
(count-pairs first)  ;; => 4 

(define third7 (cons 'a 'b)) 
(define second7 (cons third7 third7)) 
(define first7 (cons second7 second7)) 
(count-pairs first7)  ;; => 7 

(define lst (list 'a 'b 'c)) 
(set-cdr! (cddr lst) lst) 
;(count-pairs lst)  ;; never returns 

(define xx (list 1))

(count-pairs (list xx xx))
