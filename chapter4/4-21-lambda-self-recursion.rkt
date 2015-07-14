#lang racket

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)

(define f (lambda (n)
            ((lambda (fact)
               (fact fact n))
             (lambda (ft k)
               (if (= k 1)
                   1
                   (* k (ft ft (- k 1))))))))

(define fib (lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (self k)
                 (cond ((= k 1) 1)
                       ((= k 0) 1)
                       (else (+ (self self (- k 1)) (self self (- k 2)))))))))

;(fib 100)

(define (fib-simple n) 
  (define (iter a b i)
    (cond ((= i n) a)
          (else (iter (+ a b) a (+ i 1)))))
  (iter 1 1 1))

;(fib-simple 3)

;(fib-simple 100)

(define (even?-recursion x)
  (define (even? n)
    (if (= n 0)
        true
        (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
        false
        (even? (- n 1))))
  (even? x))

(define (even-lambda x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(even-lambda 3)








