#lang racket

(require racket/stream)

(define (square x) (* x x))

(define (rand x y) (+ (* (random) (abs (- x y))) x))

;(define (monte-carlo trials experiment)
;  (define (iter rest passed)
;    (cond ((= rest 0) (/ passed trials))
;          ((experiment)
;           (iter (- rest 1) (+ passed 1)))
;          (else (iter (- rest 1) passed))))
;  (iter trials 0))

(define (p x y x0 y0 r)
  (if (<= (sqrt (+ (square (- x x0)) (square (- y y0)))) r)
      1 
      0))

;(define (estimate-intergral p x1 x2 y1 y2 trials)
;  (* (monte-carlo trials
;                  (lambda () (p 
;                              (rand x1 x2) 
;                              (rand y1 y2) 
;                              (/ (+ x2 x1) 2) 
;                              (/ (+ y1 y2) 2) 
;                              (/ (abs (- x1 x2)) 2))))
;     (* (abs (- x1 x2)) (abs (- y1 y2)))))
;
;(estimate-intergral p -1.0 1.0 -1.0 1.0 1000000)

(define (stream-add a b)
  (stream-cons (+ (stream-first a) (stream-first b))
               (stream-add (stream-rest a) (stream-rest b))))

(define (stream-div s1 s2)
  (stream-cons (/ (stream-first s1)
                  (stream-first s2))
               (stream-div (stream-rest s1)
                           (stream-rest s2))))

(define (stream-scale stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (stream-front-show stream n)
  (define (iter i)
    (if (>= i n)
        (begin (display (stream-ref stream i))
               (newline))
        (begin (display (stream-ref stream i))
               (newline)
               (iter (+ i 1)))))
  (iter 0))

(define ones (stream-cons 1 ones))

(define integers (stream-cons 1.0 (stream-add integers ones)))

(define (estimate-integral p x1 x2 y1 y2)
  (define 1-0 (stream-cons (p (rand x1 x2)
                              (rand y1 y2)
                              (/ (+ x2 x1) 2)
                              (/ (+ y2 y1) 2)
                              (/ (abs (- x1 x2)) 2))
                           (stream-map (lambda (x) (p (rand x1 x2)
                                                     (rand y1 y2)
                                                     (/ (+ x2 x1) 2)
                                                     (/ (+ y2 y1) 2)
                                                     (/ (abs (- x1 x2)) 2)))
                                       ones)))
  (define pass-num (stream-cons (stream-first 1-0)
                              (stream-add pass-num (stream-rest 1-0))))
  (define result (stream-div pass-num integers))
  result)

(define test (stream-scale (estimate-integral p -1.0 1.0 -1.0 1.0)
                           4))

(stream-front-show test 5000)
                                