(newline)
(display "-----------1.45------------")
(newline)

(define (square x) (* x x)) 

(define (repeated f k)
        (if (<= k 1)
            f
            (compose f (repeated f (- k 1)))))




 (define tolerance 0.00001) 
  
 (define (fixed-point f first-guess) 
   (define (close-enough? v1 v2) 
     (< (abs (- v1 v2)) tolerance)) 
   (define (try guess) 
     (let ((next (f guess))) 
       (if (close-enough? guess next) 
           next 
           (try next)))) 
   (try first-guess)) 

(define (fast-expt base exp)
        (cond ((= exp 0) 1)
              ((even? exp) (square (fast-expt base (/ exp 2))))
              (else (* base (fast-expt base (- exp 1))))))

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








 (define (get-max-pow n) 
   (define (iter p r) 
     (if (< (- n r) 0) 
         (- p 1) 
         (iter (+ p 1) (* r 2)))) 
    
   (iter 1 2)) 
  




 (define (pow b p) 
   (define (even? x) 
     (= (remainder x 2) 0)) 
    
   (define (sqr x) 
     (* x x)) 
    
   (define (iter res a n) 
     (if (= n 0) 
         res 
         (if (even? n) 
             (iter res (sqr a) (/ n 2)) 
             (iter (* res a) a (- n 1))))) 
    
   (iter 1 b p)) 
  


 (define (repeated f n) 
   (if (= n 1) 
       f 
       (lambda (x) (f ((repeated f (- n 1)) x))))) 



 (define (nth-root n x) 
   (fixed-point ((repeated average-damp (get-max-pow n)) 
                 (lambda (y) (/ x (fast-expt y (- n 1))))) 
                1.0)) 

(display (nth-root 7 128))




(define (5-sqrt x) 
        (fixed-point 
            (n-average (lambda (y) (/ x (fast-expt y 7))) 3 ) ;3 average-damp
            300.0 ))

(display (5-sqrt 256.0));2
(newline)

(display (5-sqrt 243.0));3
(newline)

(newline)
(display "-----------1.46------------")
(newline)
