(newline)
(display "---------------------------")
(newline)

;the common sum template:

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) (* x x x))

;(display (cube 3))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(display (sum-cubes 1 3))
(newline)

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (sum pi-term a pi-next b))

(display (* 8 (pi-sum 1 10000)))

(newline)
(display "-----------1.29------------")
(newline)
(define (simpson a b n) 
  (define (simp-term x) 
    (let ((k (round (/ (* (- x a) n) (- b a))))) 
      (cond ((= k 0) (cube x)) 
            ((= k n) (cube x))
            ((even? k) (* 2 (cube x)))
            (else (* 4 (cube x))))))
  (define (simp-next x) (+ x (/ (- b a) n)))
  (* (sum simp-term a simp-next b) 
     (/ (- b a) (* 3 n))))

(display (simpson 0 1.0 100))
(newline)

(display (simpson 0 1.0 1000))
(newline)

(display (simpson 0 1.0 10000))
(newline)

(display (simpson 0 1.0 10000))
(newline)

;new function learned in this question 
;(floor x) (round x)
;(inexact->exact) turn a float to rational

(newline)
(display "-----------1.30------------")
(newline)

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(display (simpson 0 1.0 100))
(newline)

(display (simpson 0 1.0 1000))
(newline)

(display (simpson 0 1.0 10000))
(newline)

(display (simpson 0 1.0 10000))
(newline)

;works well

(newline)
(display "-----------1.31------------")
(newline)

(define (square x) (* x x))

;the recursive process:

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (pi-term a) (/ (* (- a 1) (+ a 1)) (square a)))

(define (pi-next a) (+ a 2))

(define (pi-product a b) (product pi-term a pi-next b))

(display (* (pi-product 3.0 1000.0) 4))
(newline)

(display (* (pi-product 3.0 10000.0) 4))
(newline)

;the iterative process :

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1.0))

(display (* (pi-product 3.0 1000.0) 4))
(newline)

(display (* (pi-product 3.0 10000.0) 4))
(newline)

(newline)
(display "-----------1.32------------")
(newline)

;recursive process:

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value 
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b) (accumulate + 0 term a next b))

(display (simpson 0 1.0 1000))
(newline)

(define (product term a next b) (accumulate * 1 term a next b))

(display (* (pi-product 3.0 1000.0) 4))
(newline)

(newline)
(display "---------------------------")
(newline)

;recursive process:

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) 
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum term a next b) (accumulate + 0 term a next b))

(display (simpson 0 1.0 1000))
(newline)

(define (product term a next b) (accumulate * 1 term a next b))

(display (* (pi-product 3.0 1000.0) 4))
(newline)

(newline)
(display "-----------1.33------------")
(newline)

;I will just write the iterative method:

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (cond ((> a b) result)
          ((not (filter a b)) (iter (next a) result))
          (else (iter (next a) (combiner result (term a))))))
  (iter a null-value))

(newline)
(display "-----------1.33a------------")
(newline)

(define (inc x) (+ x 1))

(define (square x) (* x x))

;Here I use the Miller-Rabin test to determine whether a number is a prime:

(define (non-trival-sqrt? n m)
  (cond ((= n 1) #f)
        ((= n (- m 1)) #f)
        (else (= (remainder (square n) m) 1))))

(define (expmod base exp m)
  (cond ((= exp 0) (remainder 1 m))
        ((even? exp)
         (let ((x (expmod base (/ exp 2) m)))
           (if (non-trival-sqrt? x m) 
               0 
               (remainder (square x) m)))) 
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (mr-test n)
  (define (try-it a) (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))



(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((mr-test n) (fast-prime? n (- times 1))); use the mr test to determine the primality.
        (else false)))

(define (prime? n t) (fast-prime? n 10)) ;the fileter function needs two arguments, though here one of them is useless.

;above is the defination of "prime?", next is the defination of the answer:

(define (sum-square-prime a b) (filtered-accumulate + 0 square a inc b prime?))

(display (sum-square-prime 2 10))

(newline)
(display "-----------1.33b------------")
(newline)

(define (identity x) x)

(define (relative-prime? a b) (= (gcd a b) 1) )

(define (product-relative-prime n) (filtered-accumulate * 1 identity 1 inc n relative-prime?))

(display (product-relative-prime 10))  

(newline)
(display "-----------1.34------------")
(newline)

(define (f g) (g 2))

(display (f square))

(display (f (lambda (z) (* z (+ z 1)))))

;(display (f f))

;(f f) => (f 2), (f 2) is illegal because f needs a procedure to be its argument.

(newline)
(display "-----------1.35------------")
(newline)

(define (close-enough? a b)
  (< (abs (- a b) ) 0.00000001))

(define (fixed-point f guess)
  (let ((x (f guess)))
    (if (close-enough? x guess) 
        x
        (fixed-point f x)))) ;this procedure is much simpler than that on the book.

(display (fixed-point cos 1.0))
(newline)

(display (fixed-point (lambda (x) (+ (/ 1 x) 1)) -5.0))

(newline)
(display "-----------1.36------------")
(newline)

;without average damping:

(define (fixed-point f guess)
  (let ((x (f guess)))
    (if (close-enough? x guess)
        (begin (newline) (display x) x)
        (begin (newline) (display guess) (fixed-point f x)))))


(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(newline)

;with average damping:

(define (average a b) (/ (+ a b) 2.0))

(define (fixed-point f guess)
  (let ((x (average guess (f guess))))
    (if (close-enough? x guess)
        (begin (newline) (display x) x)
        (begin (newline) (display guess) (fixed-point f x)))))


(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(newline)

;it is obvious that using the average damping takes much less steps.

(newline)
(display "-----------1.37a------------")
(newline)

;recursive process:

(define (cont-frac n d k)
  (define (iter i)
    (if (>= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100)) 
(newline)

(newline)
(display "-----------1.37b------------")
(newline)

;iterative process:

(define (cont-frac n d k)
  (define (iter ni di k)
    (if (<= k 1)
        (/ (ni 1) (di 1))
        (iter ni 
              (lambda (k) 
                (+ (d k) 
                   (/ (ni (+ k 1)) 
                      (di (+ 1 k))))) ;change the procedure "di" iteratively, 
              (- k 1))))                    ;in this case, should look from up to down,
  (iter n d k))                                   ;which means every level need the arguments to plus 1.

(display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100)) 
(newline)

(newline)
(display "-----------1.38------------")
(newline)


(define (d i)
  (cond ((= (remainder i 3) 1) 1.0)
        ((= (remainder i 3) 0) 1.0)
        ((= (remainder i 3) 2) (* (/ (+ i 1) 3.0) 2.0))))

(define (dfc k) (cont-frac (lambda (i) 1.0) d k))
(newline)
(display (+ (dfc 100) 2)) 

(newline)
(display "-----------1.39------------")
(newline)

(define (tan-cf x k) 
  (cont-frac 
   (lambda (i) (cond ((= i 1) x) 
                     (else (- (* x x))))) 
   (lambda (i) (- (* 2 i) 1)) 
   k))

(display (tan-cf (/ 3.14 4) 100))
(newline)

(newline)
(display "-----------test------------")
(newline)

(define (fixed-point f guess)
  (let ((x (f guess)))
    (if (close-enough? x guess) 
        x
        (fixed-point f x)))) ;this procedure is much simpler than that on the book.

(define (average a b) (/ (+ a b) 2))

(define (average-damp f) (lambda (x) (average x (f x))))

(define (average-damp f) 
  (define (g x) (average x (f x)))
  g)

(define (sqrt-cont x) 
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
(newline)
(display (sqrt-cont 4)) 
(newline)

(display (sqrt 4))

;here is some functons construct to test the use of fixed point.

(define (sqrt-cont x)
  (fixed-point (average-damp (lambda (y) (sqrt (* (sqrt x) y)))) 1.0))

(newline)
(display (sqrt-cont 4)) 
(newline)

(define (multi-2 x) 
  (fixed-point (average-damp (lambda (y) (+ x (/ y 2)))) 1.0))

(newline)
(display (multi-2 4)) 
(newline)

(newline)
(display "-----------1.40------------")
(newline)

(define dx 0.0001)

(define (deriv g)
  (lambda (x) 
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g) (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c) (lambda (x) (+ (* x x x) (* a x x) (* b x) c))) 

(define (f3 a b c) (newton-method (cubic a b c) 1))

(display (f3 1 0 0))
(newline)

;different first guess give different answer

(define (f3 a b c) (newton-method (cubic a b c) -1))

(display (f3 1 0 0))
(newline)

(newline)
(display "-----------1.41------------")
(newline)

(define (double f) (lambda (x) (f (f x))))

(define (inc x) (+ x 1))
(display
 (((double (double double)) inc) 5))

(newline)
(display "-----------1.42------------")
(newline)

(define (compose f g) (lambda (x) (f (g x))))

(display 
 ((compose square inc) 6))

(newline)
(display "-----------1.43------------")
(newline)

(define (repeated f k) 
  (if (<= k 1) 
      f
      (lambda (x) (f ((repeated f (- k 1)) x)))))

(display ((repeated square 2) 5))
(newline)
;use the "compose"

(define (repeated f k)
  (if (<= k 1)
      f
      (compose f (repeated f (- k 1)))))

(display ((repeated square 2) 5))
(newline)   

(newline)
(display "-----------1.44------------")
(newline)

(define (smooth f) (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-smooth f n) (repeated smooth n))

(newline)
(display "-----------1.45------------")
(newline)




(define (fast-expt base exp)
  (define (iter res base exp)
    (cond ((= exp 0) res)
          ((even? exp) (iter res (square base) (/ exp 2)))
          (else (iter (* res base) base (- exp 1)))))
  (iter 1 base exp)) 

(define (average-damp f)
  (lambda (y) (/ (+ y (f y)) 2)))

(define (n-average f n) 
  ((repeated average-damp n) f))

(define (4-sqrt x) 
  (fixed-point 
   (lambda 
       (y) 
     ((n-average (lambda (y) (/ x (fast-expt y 3))) 2.0) y )) ;2 average-damp
   300.0 ))

(display (4-sqrt 256));4
(newline)

(display (4-sqrt 81));3
(newline)

(define (5-sqrt x) 
  (fixed-point 
   (n-average (lambda (y) (/ x (fast-expt y 4))) 2) ;3 average-damp
   1.0 ))

(display (5-sqrt 32.0));2
(newline)

(display (5-sqrt 243.0));3
(newline)

(define (8-sqrt x) 
  (fixed-point 
   (n-average (lambda (y) (/ x (fast-expt y 7))) 3) ;3 average-damp
   30.0 ))

(display (8-sqrt 256.0));2
(newline)

(display (8-sqrt 6561.0));3
(newline)

; the regulation is to find the 2^n small than x, and use average damp n times.

(define (exp2 x)
  (define (iter n r)
    (if (> r x) (- n 1) (iter (+ n 1) (* r 2))))
  (iter 1 2))

(define (root-n x n)
  (fixed-point
   (n-average (lambda (y) (/ x (fast-expt y (- n 1)))) (exp2 n))
   1.0));fix me:here must be float, or it will be wrong.
(newline)
(display (root-n 256 8))
(newline)
(display "-----------1.46------------")
(newline)

(define (iterative-improve good? improve)
  (define (iter x)
    (if (good? x)
        (improve x)
        (iter (improve x))))
  iter)

(define (sqrt1 x)
  (define (improve guess) (/ (+ (/ x guess) guess) 2))
  (define (good? guess) (< (abs (- guess (improve guess))) 0.00001))
  ((iterative-improve good? improve) x))

(display (sqrt1 4.0))

(define (fixed-point f guess)
  (define (good? guess) (< (abs (- guess (f guess))) 0.00001))
  ((iterative-improve good? f) guess))

;choose a function using fixed-point to test.

(define (root-n x n)
  (fixed-point
   (n-average (lambda (y) (/ x (fast-expt y (- n 1)))) (exp2 n))
   1.0));fix me:here must be float, or it will be wrong.
(newline)
(display (root-n 256 8))
(newline)
