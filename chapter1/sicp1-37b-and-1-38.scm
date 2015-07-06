(newline)
(display "-----------1.37b------------")
(newline)

;iterative process:

(define (cont-frac n d k)
  (define (iter ni di k)
    (if (<= k 1)
        (/ (ni 1) (di 1))
        (begin (display (di 3)) (newline) (iter ni (lambda (k) (+ (d k) (/ (ni (+ k 1)) (di (+ 1 k))))) (- k 1)))))
  (iter n d k))

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
