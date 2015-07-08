#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define flip-horiz1 
  (transform-painter (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(paint einstein)

(paint (flip-horiz1 einstein))

(paint (rotate90 einstein))

(define rotate180_
  (lambda (painter) (rotate90 (rotate90 painter))))

(paint (rotate180_ einstein))

(define rotate270_
  (lambda (painter) (rotate90 (rotate180 painter))))

(paint (rotate270_ einstein))