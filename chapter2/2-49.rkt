#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define v1 (make-vect 0.5 0))

(define v2 (make-vect 0 0.5))

(define o (make-vect 0.5 0.5))

(define f (make-frame o v1 v2)) 

(define (frame-outline frame)
  (segments->painter 
   (let ((o (frame-origin frame)) 
         (v1 (frame-edge1 frame)) 
         (v2 (frame-edge2 frame))) 
    (list (make-segment o
                       (vector-add o v1))
          (make-segment o
                       (vector-add o v2))))))

(paint (frame-outline f))

(define x-painter 
  (segments->painter 
   (list (make-segment (make-vect 0 0)
                       (make-vect 1 1))
         (make-segment (make-vect 0 1)
                       (make-vect 1 0)))))

(paint x-painter)

(define mid1 (make-vect 0 0.5))

(define mid2 (make-vect 0.5 1))

(define mid3 (make-vect 1 0.5))

(define mid4 (make-vect 0.5 0))

(define diomend
  (segments->painter
   (list (make-segment mid1 mid2)
         (make-segment mid2 mid3)
         (make-segment mid3 mid4)
         (make-segment mid4 mid1))))

(paint diomend)

;omit the wave painter...