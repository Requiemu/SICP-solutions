(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x)) 

(define (add-interval x y) 
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y) 
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

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

(define (width-interval x) (- (upper-bound x) (lower-bound x)))

(display (div-interval (make-interval 5 10)
                       (make-interval -1 1)));this has no meaning
(newline)

(define (div-interval-modify x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)  
      (begin (display "error") (newline) #f)  ;give a "error" signal
      (div-interval x y))) 

(display (div-interval-modify (make-interval 5 10)
                       (make-interval -1 1)))
(newline)
