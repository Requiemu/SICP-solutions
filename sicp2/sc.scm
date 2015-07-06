#! /usr/local/bin/guile -s
!#
(+ 4 5)
(display "hello world\n")
(define a "hello")
(display a )
(display #\newline)
(display "hello world")	
(define (fac x) 
                (cond ((<= x 1) 1) 
                (else (* x (fac (- x 1))))));factorial recursion style.
(define s (fac 4))
(display (number->string (fac 4)))
(display "\n")
(define (fac n) 
        (define (iter product counter)
        (if (> counter n) product (iter (* counter product) (+ counter 1))))
           (iter 1 1))
(display (number->string (fac 5)))
(display "\n")



(define (fac n)
               (define (iter i) (cond ((< i n) (* i (iter (+ i 1)))) ((>= i n) n)))
                (iter 1))
(display (number->string (fac 5)))   ;factorial, another recursion solution.
(display "\n")




(define (f x y) (cond ((< x y) 
                         (begin (display (number->string x) ) (f (+ x 1) y)))
                     ((>= x y)
                          (display (number->string y) ))))  ;loop
(f 1 10)


(define (f x y) (if (<= x y) (begin (display x) (display #\space) (f (+ x 1) y))))
(f 1 10)
(display #\newline)
(display "--------------------------")
(let ((x 2) (y 5)) 
      (let* ((x 6) (z (+ x y)))
           (display (* z x))))
(display #\newline)
(display "--------------------------")
(let ((x 2) (y 5)) 
      (let ((x 6) (z (+ x y)))
           (display (* z x))))
(display #\newline)
(display "--------------------------")
(display #\newline)
(let ((x 2) (y 3)) (let ((z (+ x y))) (display z)))
(let* ((x 2) (y 3) (z (+ x y))) (display z))
(letrec ((z (+ x y))(x 2) (y 3) ) (display z)) ;let let* letrec
;(let ((x 2)) let ((y 3)) (display (+ x y))) 
(display (apply + (list 2 3 4)))
(define f (lambda (x) (apply + x)))
(display (f (list 1 2 3)))
(define sum (lambda (x) (apply + x)))
(display (sum (list 1 2 3)))
(define avg (lambda (x) (/ (sum x) (length x))))
(display (avg (list 1 2 4)))  ; apply (list x y z)
(display (map length '((list 1 2 30 30) (list 2 3 44))));'((1 2 3) (4 5 6)) is ((1 2 3) (4 5 6)), but (list (1 2 3) (4 5 6)) is illegal!
(display (map length (list (list 1 2 30 30) (list 2 3 44))))
(display (equal? '((list 1 2 30 30) (list 2 3 44)) 
                  (list (list 1 2 30 30) (list 2 3 44))));apply map
(display #\newline)
(display "--------------------------")
(display #\newline)
(define port (open-input-file "string"))
(display port)
(display (read port))
(define a (read))
(display a)
(close-input-port port)
(define port (open-output-file "string"))
(write "helloworlde"  port)
(close-output-port port)
(display #\newline)
(display "-----------1.2------------")
(display #\newline)
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) ( - 2 7)))
(use-modules (ice-9 pretty-print))
(pretty-print '((/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) ( - 2 7)))));1.2
(display #\newline)
(display "-----------1.3------------")
(display #\newline)
(define (sq-of-2 x y z) (-(+ (* x x) (* y y) (* z z)) (* (min x y z) (min x y z))))
(display (sq-of-2 3 5 7))
(display #\newline)
(display "--------------------------")
(display #\newline)
(define (newton x n m) 
        (if (= m 0) (/(+(/ x n) n) 2) 
            (newton x (/(+(/ x n) n) 2) (- m 1))))




(display (newton 2 1. 10))
(display #\newline)
(display "--------------------------")
(display #\newline)
(define a (cons 'a 'b))
(display a)
(define (f a) (set-car! a 'c))
(f a)
(display a)
(define b 1)
(display b)
(define (f b) (set! b 2))
(display b)




