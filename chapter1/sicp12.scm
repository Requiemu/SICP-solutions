(newline)
(display "-----------1.19------------")
(newline)
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count) 
         (fib-iter a 
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
         (else 
               (fib-iter (+ (* b q) (* a q) (* a p))
                         (+ (* b p) (* a q))
                         p
                         q
                         (- count 1)))))

(display (fib 20))

(newline)

(define (smallest-divisor n)
           (find-divisor n 2))

(define (find-divisor n k)
        (cond ((> (* k k) n) n)
              ((divided? n k) k)
              (else (find-divisor n (+ k 1)))))

(define (divided? n k)
        (= (remainder n k) 0))

(display (smallest-divisor (* 7 13)))

(define (prime? n)
  (= n (smallest-divisor n)))

(display (prime? 91))

(newline)
(display "-----------1.21------------")
(newline)

(display (smallest-divisor 199))
(newline)

(display (smallest-divisor 1999))
(newline)

(display (smallest-divisor 19999))
(newline)

(newline)
(display "-----------1.22------------")
(newline)

;racket has no runtime, here you can use current-milliseconds to do the same thing.

(define runtime current-inexact-milliseconds)

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

;I add a return value(#t and #f) to the timed function, so we can know the primality outside the timed functions.

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      #f))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
  #t)

(define (search-for-primes first) 
   (define (search-iter num i) 
      (cond ((>= i 3) (void))
            ((timed-prime-test num) (search-iter (+ num 2) (+ i 1)))
            (else (search-iter (+ num 2) i))))
   (search-iter (if (even? first) (+ first 1) first) 0))

; search the three smallest number after the first number.

(search-for-primes 1000)     ; 1e3 
(search-for-primes 10000)    ; 1e4 
(search-for-primes 100000)   ; 1e5 
(search-for-primes 1000000)  ; 1e6 
  
; the number above didn't show effect obviously, the number should be bigger:
 
(newline) 
(search-for-primes 100000000000)     ; 1e9  
(search-for-primes 1000000000000)    ; 1e10 
(search-for-primes 10000000000000)   ; 1e11 
(search-for-primes 100000000000000)  ; 1e12 
(newline)

;new function learned in this question: (void) (newline) (current-milliseconds)

(newline)
(display "-----------1.23------------")
(newline)
(define (next n)
          (if (= n 2) 3 (+ n 2)))

(define (smallest-divisor n)
           (find-divisor n 2))

(define (find-divisor n k)
        (cond ((> (* k k) n) n)
              ((divided? n k) k)
              (else (find-divisor n (next k)))))

(define (divided? n k)
        (= (remainder n k) 0))

(display (smallest-divisor (* 7 13)))

(define (prime? n)
  (= n (smallest-divisor n)))

(search-for-primes 1000)     ; 1e3 
(search-for-primes 10000)    ; 1e4 
(search-for-primes 100000)   ; 1e5 
(search-for-primes 1000000)  ; 1e6 
  
; the number above didn't show effect obviously, the number should be bigger:
 
(newline) 
(search-for-primes 100000000000)     ; 1e9  
(search-for-primes 1000000000000)    ; 1e10 
(search-for-primes 10000000000000)   ; 1e11 
(search-for-primes 100000000000000)  ; 1e12 
;the result is not precisely 2 time, but the method do save some time.
;the above program implicit that the version of function is related to the position where the function is called.

(newline)
(display "-----------1.24------------")
(newline)

;modify the end part of this function in the book, or it will work wrong in (expmod 1 0 1)
(define (square n) (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) (remainder 1 m))
        ((even? exp)
        (remainder (square (expmod base (/ exp 2) m))
                   m))
        (else
        (remainder (* base (expmod base (- exp 1) m))
                   m))))

;(display (expmod 1 0 1))

(define (fermat-test n)
  (define (try-it a) (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(display (fermat-test 5))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime n (- (runtime) start-time))
      #f))

(search-for-primes 1000)     ; 1e3 
(search-for-primes 10000)    ; 1e4 
(search-for-primes 100000)   ; 1e5 
(search-for-primes 1000000)  ; 1e6 
  
; the caculation in this method is so quick, however we can see some regulation in it. 
 
(newline) 
(search-for-primes 1000000)      
(search-for-primes 10000000)  
(search-for-primes 100000000)    
(search-for-primes 1000000000)  

(newline)
(display "-----------1.25------------")
(newline)

(define (fast-expt base exp)
        (cond ((= exp 0) 1)
              ((even? exp) (square (fast-expt base (/ exp 2))))
              (else (* base (fast-expt base (- exp 1))))))

;(display (fast-expt 3 5))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(search-for-primes 1000)     ; 1e3 
(search-for-primes 10000)    ; 1e4 
;(search-for-primes 100000)   ; 1e5 
;(search-for-primes 1000000)  ; 1e6 
 
;(newline) 
;(search-for-primes 1000000)      
;(search-for-primes 10000000)  
;(search-for-primes 100000000)    
;(search-for-primes 1000000000)  these line will take very long time.

;this opinion is obviously wrong, because in this case (fast-expt base exp) will make a very huge number, and the method before decrease the number on the run.

(newline)
(display "-----------1.26------------")
(newline)

;This is because (square (x)) just need to evaluate (x) once ,(* (x) (x)) need  to evaluate (x) twice.

(newline)
(display "-----------1.27------------")
(newline)
(define (expmod base exp m)
  (cond ((= exp 0) (remainder 1 m))
        ((even? exp)
        (remainder (square (expmod base (/ exp 2) m))
                   m))
        (else
        (remainder (* base (expmod base (- exp 1) m))
                   m))))

(define (all-fermat-test n)
        (define (iter n i)
                (cond ((= n i) #t) 
                      ((not (= (expmod i n n) i)) #f) 
                      (else (iter n (+ i 1)))))
        (iter n 1))
(display (all-fermat-test 5))
(display (all-fermat-test 24))
(display (all-fermat-test 13))
(display (all-fermat-test 561))
(display (all-fermat-test 1105))
(display (all-fermat-test 1729))
(display (all-fermat-test 2465))
(display (all-fermat-test 2821))
(display (all-fermat-test 6601))

;the Carmichael number all fooled the test!

(newline)
(display "-----------1.28------------")
(newline)
(define (square x) (* x x))

(define (non-trival-sqrt? n m)
        (cond ((= n 1) #f)
              ((= n (- m 1)) #f)
              (else (= (remainder (square n) m) 1))))

;(display (non-trival-sqrt? 3 5)) 

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

(define (all-mr-test n)
        (define (iter n i)
                (cond ((= n i) #t) 
                      ( (not (= (expmod i (- n 1) n) 1)) #f) 
                      (else (iter n (+ i 1)))))
        (iter n 1))
(display (all-mr-test 5))
(display (all-mr-test 24))
(display (all-mr-test 13))
(display (all-mr-test 561))
(display (all-mr-test 1105))
(display (all-mr-test 1729))
(display (all-mr-test 2465))
(display (all-mr-test 2821))
(display (all-mr-test 6601))

;This program can't be fool. 
;Two point: one is "equal to 1", one is "not equal to one"(in the "expmod").

