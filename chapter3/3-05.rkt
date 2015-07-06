#lang racket

(define (square x) (* x x))

(define (rand x y) (+ (* (random) (abs (- x y))) x))

(define (monte-carlo trials experiment)
  (define (iter rest passed)
    (cond ((= rest 0) (/ passed trials))
          ((experiment)
           (iter (- rest 1) (+ passed 1)))
          (else (iter (- rest 1) passed))))
  (iter trials 0))

(define (p x y x0 y0 r)
  (<= (sqrt (+ (square (- x x0)) (square (- y y0)))) r))

(define (estimate-intergral p x1 x2 y1 y2 trials)
  (* (monte-carlo trials
                  (lambda () (p 
                              (rand x1 x2) 
                              (rand y1 y2) 
                              (/ (+ x2 x1) 2) 
                              (/ (+ y1 y2) 2) 
                              (/ (abs (- x1 x2)) 2))))
     (* (abs (- x1 x2)) (abs (- y1 y2)))))

(estimate-intergral p -1.0 1.0 -1.0 1.0 1000000)

