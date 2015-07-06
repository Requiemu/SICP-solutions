(newline)
(display "-----------2.1------------")
(newline)


(define (gcda a b) 
        (if (= b 0) 
            a
            (gcda (abs (- a b)) (abs (min a b)))));a gcd function which can caculate negative

(display (gcda 6 4))
(newline)
 
(display (gcda -18 6))
(newline)

(define (make-rat a b) 
        (let ((g (gcda a b)))
             (if (> (* a b) 0)  
             (cons (abs (/ a g)) (abs (/ b g)))
             (cons (- (abs (/ a g))) (abs (/ b g))))))

(display (make-rat -1 -1))
(newline)

(display (make-rat 6 -8))
(newline)

(display (make-rat 13 26))
(newline)

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
   (newline)
   (display (numer x))
   (display "/")
   (display (denom x)))

(define (div-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y)))) 

(newline)
(display "-----------2.2------------")
(newline)

(define (average x y) (/ (+ x y) 2))

(define (make-point x y) (cons x y))

(define (x-point a) (car a))

(define (y-point a) (cdr a))

(define (make-segment a b) (cons a b))

(define (start-segment a) (car a))

(define (end-segment a) (cdr a))

(define (midpoint-segment v) 
        (make-point (average (x-point (start-segment v)) (x-point (end-segment v)))
                    (average (y-point (start-segment v)) (y-point (end-segment v)))))

(define (print-point p)
   (newline)
   (display "(")
   (display (x-point p))
   (display ".")
   (display (y-point p))
   (display ")"))

(define a (make-point 2.0 3.0))

(define b (make-point 4.0 5.0))

(define m (midpoint-segment (make-segment a b)))

(print-point m)

(newline)
(display "-----------2.3------------")
(newline)

(define (make-rec left-top right-down) (cons left-top right-down))

(define (left-top a) (car a))

(define (right-down a) (cdr a))

(define (left-down a) (make-point (x-point (left-top a)) (y-point (right-down a))))

(define (right-top a) (make-point (x-point (right-down a)) (y-point (left-top a))))

;above is the basic operation for rectangle

(define (perimeter-rec a) 
  (let ((length (abs (- (x-point (right-down a)) (x-point (left-top a)))))
        (width (abs (- (y-point (left-top a)) (y-point (right-down a))))))
        (* (+ length width) 2)))

(define (area-rec a) 
  (let ((length (abs (- (x-point (right-down a)) (x-point (left-top a)))))
        (width (abs (- (y-point (left-top a)) (y-point (right-down a))))))
       (* length width)))

(display (perimeter-rec (make-rec (make-point 0 0) (make-point 5 5))))
(newline)

(display (area-rec (make-rec (make-point 3 4) (make-point 5 5))))
(newline)

(newline)
(display "-----------2.4------------")
(newline)

;this is very interesting

(define (cons x y) (lambda (m) (m x y)))

(define (car a) (a (lambda (x y) x)))

(define (cdr a) (a (lambda (x y) y)))

(define a (cons 3 4))

(display (car a))
(newline)

(display (cdr a))
(newline)

(newline)
(display "-----------2.5------------")
(newline)

(define (cons a b) (* (expt 2 a) (expt 3 b)))

(define (car z) 
   (define (iter x i)
   (if (odd? x)
       i
       (iter (/ x 2) (+ i 1))))
   (iter z 0))

(define (cdr z)
   (define (iter x i)
   (if (not (= (remainder x 3) 0))
       i
       (iter (/ x 3) (+ i 1))))
   (iter z 0))

(define a (cons 3 4))

(display (car a))
(newline)

(display (cdr a))
(newline)

(newline)
(display "-----------2.6------------")
(newline)

(define (inc x) (+ x 1))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b) (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(display (((add one two) inc) 2))
(newline)

(newline)
(display "-----------2.7------------")
(newline)

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x)) 

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) 
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                              (/ 1.0 (lower-bound y)))))

(display (div-interval (make-interval 1.0 1.0) (make-interval 2.0 2.0)))
