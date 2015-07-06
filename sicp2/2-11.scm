(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x)) 

(define (mul-interval x y)
  (let ((p1 (lower-bound x))
        (p2 (upper-bound x))
        (p3 (lower-bound y)) 
        (p4 (upper-bound y))
        (b1 (> (lower-bound x) 0))
        (b2 (> (upper-bound x) 0))
        (b3 (> (lower-bound y) 0)) 
        (b4 (> (upper-bound y) 0)))
    (cond ((and b1 b2 b3 b4)             (make-interval (* p1 p3) (* p2 p4)))
          ((and b1 b2 (not b3) b4)       (make-interval (* p2 p3) (* p2 p4)))
          ((and b1 b2 (not b4))          (make-interval (* p2 p3) (* p1 p4)))
          ((and (not b1) b2 (not b3) b4) (make-interval (min (* p2 p3) (* p1 p4)) (max (* p2 p4) (* p1 p3))))
          ((and (not b1) b2 (not b3) (not b4)) (make-interval (* p2 p3) (* p1 p3)))
          ((and (not b1) (not b2) (not b3) (not b4)) (make-interval (* p2 p4) (* p1 p3)))
          (else (mul-interval y x)))))

;so long...

(display (mul-interval (make-interval -2 4) (make-interval -4 -2)))
(newline)
