#lang racket

(define (integers-between m n)
  (if (>= m n) (cons m '())
      (cons m (integers-between (+ m 1) n))))

(define (integers n)
  (integers-between 1 n))

(define (first-conflict? listn)
  (let ((first (car listn)))
    (define (iter m num)
      (if (null? m) #t
          (and (not (= (abs (- first (car m)))))
               (not (= first (car m)))
               (iter (cdr m) (+ num 1)))))
    (iter (cdr listn 1))))

(define (conflict? listn)
  (if (null? listn) #t
      (and (first-conflict? listn)
           (conflict? (cdr listn)))))

(define (solutions)
  (let ((a (apply amb (integers 8)))
        (b (apply amb (integers 8)))
        (c (apply amb (integers 8)))
        (d (apply amb (integers 8)))
        (e (apply amb (integers 8)))
        (f (apply amb (integers 8)))
        (g (apply amb (integers 8)))
        (h (apply amb (integers 8))))
    (require (conflict? (list a b c d e f g h)))
    (list a b c d e f g h)))
                 