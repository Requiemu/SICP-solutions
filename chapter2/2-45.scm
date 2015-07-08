#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
 
(define (split first second)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split first second) painter (- n 1))))
          (first painter (second smaller smaller))))))

(define right-split (split beside below))

(paint (right-split einstein 3))

(define up-split (split below beside))

(paint (up-split einstein 3))