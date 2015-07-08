#lang racket
;(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin frame)
  (car frame))

(define (edge1 frame)
  (cadr frame))

(define (edge2 frame)
  (cddr frame))

(define a (make-frame 1 2 3))

(display (origin a))

(display (edge1 a))

(display (edge2 a))