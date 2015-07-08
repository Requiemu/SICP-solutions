#lang racket
;(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin frame)
  (car frame))

(define (edge1 frame)
  (cadr frame))

(define (edge2 frame)
  (caddr frame))

