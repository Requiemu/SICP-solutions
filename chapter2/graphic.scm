#lang slideshow
(define (square x) (rectangle x x))

(define (sqiter n) 
  (if (= n 1) (square 1)
      (hc-append (vc-append (square n) (square n))
                 (sqiter (floor (/ n 2.0))))))

;(sqiter 128)

(define (sqre n)
  (define (iter i)
    (if (>= i n) (vc-append (square (/ 128 (expt 2 i))) (square (/ 128 (expt 2 i))))
            (hc-append (vc-append (square (/ 128 (expt 2 i))) (square (/ 128 (expt 2 i)))) 
                       (vc-append (iter (+ i 1)) (iter (+ i 1))))))
  (iter 0.0))

(sqre 10)

(define (cire n)
  (define (iter i)
    (if (>= i n) (vc-append (circle (/ 128 (expt 2 i))) (circle (/ 128 (expt 2 i))))
            (hc-append (vc-append (circle (/ 128 (expt 2 i))) (circle (/ 128 (expt 2 i)))) 
                       (vc-append (iter (+ i 1)) (iter (+ i 1))))))
  (iter 0.0))

(cire 10)

(define (sqcor n)
  (define (iter i)
    (if (>= i n) (square (/ 128 (expt 2 i)))
        (hc-append (vc-append (square (/ 128 (expt 2 i))) (square (/ 128 (expt 2 i))))
                   (vc-append (iter (+ i 1)) (square (/ 128 (expt 2 i)))))))
  (iter 0.0))

(sqcor 20)

(define (circor n d)
  (define (iter i)
    (if (>= i n) (circle (/ d (expt 2 i)))
        (hc-append (vc-append (circle (/ d (expt 2 i))) (circle (/ d (expt 2 i))))
                   (vc-append (iter (+ i 1)) (circle (/ d (expt 2 i)))))))
  (iter 0.0))

(circor 20 128)

(define (center d)
  (hc-append (vc-append (circor 20 d) (circor 20 d))
             (vc-append (circor 20 d) (circor 20 d))))

(center 32)