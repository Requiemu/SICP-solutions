(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x)) 

(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

(define (center i) (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent x) (/ (- (upper-bound x) (lower-bound x)) (* (center x) 2)))

(display (make-center-percent 5 0.1))
(newline)

(display (percent (make-center-percent 5 0.1)))
(newline)
