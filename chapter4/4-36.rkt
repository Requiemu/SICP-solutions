#lang racket
(define (a-py-triple-start-from n)
  (let ((i (an-integer-starting-from n)))
    (let ((j (an-integer-starting-from i)))
      (let ((k (+ (square i) (square j))))
        (list i j k)))))