;;#! /usr/local/bin/guile -s
;;!#
(display #\newline)
(display "-----------1.5------------")
(display #\newline)
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
;(display (test 0 (p)))
;It will not work, cause it's applictive-order evaluation.
(display #\newline)
;newton method solve the square problem
(define (newton x n m) 
  (if (= m 0.0) (/(+(/ x n) n) 2) 
      (newton x (/(+(/ x n) n) 2) (- m 1))))
;given the iteration times
(display #\newline)
(display "-----------1.6------------")
(display #\newline)


(display (newton 2.0 1 10))
(display "\n")
(display #\newline)

(define (newton x g r) (if (< (abs(- x (* g g ))) r) g 
                           (newton x (/(+(/ x g) g) 2) r)))
;given the discretion
(display (newton 4.0 2 0.0000000001))
(display "\n")
(display #\newline)
(define square- (lambda (x) (newton x 1.0 0.000000001)));single parameter
(display (square- 2))
(display #\newline)
(display "-----------1.6------------")
(display #\newline)
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (average x y) (/(+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x) (< (abs (- (* guess guess) x)) 0.001))
(display (new-if (= 2 3) 0 5))
(display (new-if (= 2 2) 0 5))
(new-if (= 2 2) (display 1) (display 2))
(cond ((= 2 2) (display 1))
      (else (display 2)))
(if (= 2 3) (display 1) (display 2))
(display (good-enough? 1.414 2))
(display (good-enough? 2.0 2))

(display #\newline)
(display "-----------1.6------------")
(display #\newline)

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x) guess
          (sqrt-iter (improve guess x) x)))
;(display (sqrt-iter 1 2.))
(display #\newline)
(display "-----------1.6------------")
(display #\newline)

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(display (sqrt-iter 1 2.))
(display #\newline)
(display "-----------1.6------------")
(display #\newline)

(define (sqrt-iter guess x)
  (cond ((good-enough? guess x) guess )
        (else (sqrt-iter (improve guess x) x))))
(display (sqrt-iter 1 2.))
(display #\newline)
(display "-----------1.6------------")
(display #\newline)
(cond ((= 2 2) (display 1))   (else (display 2)))
(cond ((= 2 ) (display 1))   (else (display 2)));if and cond will jump over the #f condition's evaluation
(new-if (= 1 1) (display 1) (display 2))         ;but since scheme is applictive-order evaluation  the new-if function here will evaluate all its arguments' value, only then it will enter the procedure, so the "display 1" and "display 2" here will all be execute! One encapsulation? it's different~
(display #\newline)
(display "-----------1.7------------")
(display #\newline)
(define square- (lambda (x) (sqrt-iter 1.0 x )));single parameter
(display (square- 2))
(display #\newline)
(display (square- 10000000))
(display #\newline)
(display (sqrt 10000000))
(display #\newline)
(display (square- 0.0000001));the method here is not good enough for small numbers
(display #\newline)
(display (sqrt 0.0000001))
(display #\newline)
;will use:good-enough, average, (improve guess x), (sqrt-iter guess x)
;the improved method:
(define (sqrt-iter guess0 guess x) (if (< (/ (abs (- guess0 guess)) guess0) 0.001) guess (sqrt-iter guess (improve guess x) x)))
(define square- (lambda (x) (sqrt-iter x 1.0 x )));single parameter
(display (square- 0.0000001))
(display #\newline)
; encapsule the "good-enough?"
(define (good-enough? guess0 guess) (< (abs (/ (- guess0 guess) guess0))  0.0001))
(define (sqrt-iter guess0 guess x) (if (good-enough? guess0 guess) guess (sqrt-iter guess (improve guess x) x)))
(define square- (lambda (x) (sqrt-iter x 1.0 x )));single parameter
(display (square- 0.0000001))
(display #\newline)
;it works better than before
(display #\newline)
(display "-----------1.8------------")
(display #\newline)
;just need to define a new "improve"
(define (improve guess x) (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
(define (good-enough? guess0 guess) (< (abs (/ (- guess0 guess) guess0))  0.0001))
(define (sqrt-iter guess0 guess x) (if (good-enough? guess0 guess) guess (sqrt-iter guess (improve guess x) x)))
(define square- (lambda (x) (sqrt-iter x 1.0 x )));single parameter
(display (square- 27.0))
;works well
(display #\newline)
(display "-----------1.9------------")
(display #\newline)
;the first is recursive, the scecond is iterative. when the result is the function itself it is iterative, when the result is consist of something else compound with the function itself, it is recursive, because in this case you will need to record everything else in the result.
(display #\newline)
(display "-----------1.10------------")
(display #\newline)
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(display (A 1 10));1024
(display #\newline)
(display (A 2 4));65536
(display #\newline)
(display (A 3 3));65536
(display #\newline)
;(A 0 n) 2*n
;(A 1 n) 2^n
;(a 2 n) 2^2^2...^2  totally n 2
(display #\newline)
(display "---------------------------")
(display #\newline)
(define (fib n) (cond ((= n 0) 0)
                      ((= n 1) 1)
                      (else (+(fib (- n 1)) (fib (- n 2))))))
(display (fib 6)) 
;the recursive process 
(display #\newline)
(define (fib n) (define (fib-iter n1 n2 num2) 
                  (if (= num2 n) n2
                      (fib-iter n2 (+ n1 n2) (+ num2 1))))
  (fib-iter 0 1 1))
(display (fib 6)) 
;the iteration process (but recursive procedure)
(display #\newline)
;problem of changing money
(define (change-number n l) 
  (cond       ((< n (car l)) 0)
              ((= n (car l)) 1)
              ((and  (= (length l) 1) 
                     (= (remainder n (car l)) 0)) 
               1)
              ((and (= (length l) 1) 
                    (not (= (remainder n (car l)) 0))) 
               0)
              
              (else (+ (change-number (- n (car l)) l) 
                       (change-number n (cdr l))))))
(display (change-number 100 (list 1 5 10 25 50)));the list here must from little to big, because when the n is little than the first one, it will return the result.but we can design a "min" the improve this problem
(display #\newline)
;I should improve the solution above.Consider to the end will simplfy the recursion!
(define (change-number n l) 
  (cond       ((= n 0) 1)
              ((< n 0) 0)
              ((= (length l) 0) 0)
              
              (else (+ (change-number (- n (car l)) l) 
                       (change-number n (cdr l))))))
(display (change-number 100 (list 1 5 10 25 50)))
(display #\newline)
(display #\newline)
(display "-----------1.11------------")
(display #\newline)
(define (f n) (if (< n 3) n
                  (+ (f (- n 1)) (*(f (- n 2)) 2) (*(f (- n 3)) 3))))
(display (f 3))
(display #\newline)
(define (f n) (define (f-iter n0 n1 n2 num2) 
                (if (= num2 n) n2 
                    (f-iter n1 n2 (+ n2 (* 2 n1) (* 3 n0)) (+ num2 1))) )
  (f-iter 0 1 2 2)) 
(display (f 3))
(display #\newline)
(display "-----------1.12------------")
(display #\newline)


(define (pas-change i l0 l1)
  (if (>= i (length l0)) 
      (begin (list-set! l1 i 1) (list-set! l1 0 1))
      (begin (list-set! l1 
                        i 
                        (+ (list-ref l0 (- i 1)) 
                           (list-ref l0 i)))
             (pas-change (+ i 1) l0 l1))  ))


(define (pas-iter l0) 
  (define l1 
    (make-list (+ (length l0) 1)
               0)) 
  (begin (pas-change 1 l0 l1) l1))   ;given the above line, return the underline.just a capsulation.



(define (f n) (if (= n 1) (list 1) 
                  (pas-iter (f (- n 1)))));given the line'number, return it's content.



(define (pas0 n i) (if (= i n) (begin display (f n) (display #\newline))
                       (begin (display (f i)) (display #\newline) (pas0 n (+ i 1)))))
(define (pas n) (pas0 n 1))
(pas 10) ;iteration in the scheme is confusing...

(display #\newline)
(display "---------------------------")
(display #\newline)

(define (fast-expt b n) 
  (define (expt-iter i x)
    (cond ((= i n) x)
          ((> (* 2 i) n) (expt-iter (+ i 1) (* x b)))
          
          ((<= (* 2 i) n) (expt-iter (* i 2) (* x x)))))
  (expt-iter 1 b))
(display (fast-expt 2 10))

(display #\newline)
(display "-----------1.17------------")
(display #\newline)
(define (fast-expt b n)
  (define (iter a b i)
    (cond ((= i 0) a)
          ((even? i)  (iter a (* b b) (/ i 2)))
          (else (iter (* a b) b (- i 1)))))
  (iter 1 b n))

(display (fast-expt 2 10))

(display #\newline)
(display "-----------1.18------------")
(display #\newline)
(define (double x) (+ x x))
(define (half x) (/ x 2))
(define (* a b) 
  (cond ((= b 1) a)
        ((even? b) (double (* a (half b))))
        (else (+ (* a (- b 1)) a))))
(display (* 2 3))                                     ;recursive process


(define (* a b)
  (define (iter c a b)
    (cond ((= b 0) c)
          ((even? b) (iter c (double a) (half b)))
          (else (iter (+ c a) a (- b 1)))))
  (iter 0 a b))                                  ;iteration process 
(display (* 2 3))

(define (* a b)
  (define (iter x y)
    (cond ((= y 1) x)
          ((even? y) (iter (double x) (half y)))
          (else (+ (iter x (- y 1) ) a ))))
  (iter a b))
(display (* 2 3))                                      ;this is a combination of iteration and recursion
(display #\newline)
(display "-----------1.19------------")
(display #\newline)

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count) (display "hi2")
                       (fib-iter a 
                                 b
                                 (+ (* p p) (* q q))
                                 (+ (* 2 p q) (* q q))
                                 (/ count 2)))
        (else (display "hi") 
              (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
(define (fib n)
  (fib-iter 1 0 0 1 n))
(display (fib 6));here is a bug I don't know why. the program above works well in any other file but this one. It probably have sth to do with the fib defination ahead, but I can't find the exact reason. Or maybe its output is too long?







