#lang racket
;(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (make-vect x y) (cons x y))

(define (xcor-vect v) (car v))

(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect 
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))

(define (make-segment v1 v2) (cons v1 v2))

(define (start-segment segment) (car segment))

(define (end-segment segment) (cdr segment))

(define v1 (make-vect 1 2))

(define v2 (make-vect 3 4))

(define s (make-segment v1 v2))

(display (start-segment s))

(display (end-segment s))