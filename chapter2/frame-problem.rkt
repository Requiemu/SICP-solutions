#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(paint einstein)

(define v1 (make-vect 1 0))

(define v2 (make-vect 0 1))

(define o (make-vect 0 0))

(define f (make-frame (make-vect 1 1) (make-vect 2 0) (make-vect 0 1))) 

;(f einstein)
(paint (number->painter 0))
> (paint diagonal-shading)
> (paint-hires  (below (beside diagonal-shading
                        (rotate90 diagonal-shading))
                (beside (rotate270 diagonal-shading)
                        (rotate180 diagonal-shading))))
> (paint einstein)

(define s (make-segment v1 v2))

(paint (segments->painter (list s)))

(frame-coord-map f)

(define (transform-painter-1 painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (vector-sub (m corner1) new-origin)
                     (vector-sub (m corner2) new-origin)))))))

; rotate an image 45 degrees to the left
(define (rotate-45 painter)
  (transform-painter-1 painter
                       (make-vect 0.5 0.0)
                       (make-vect 1.0 0.5)
                       (make-vect 0.0 0.5)))

(paint (rotate-45 einstein))

;(paint (einstein 
 ;       (make-frame 
  ;       (make-vect 0.5 0.0)
   ;      (make-vect 1.0 0.5)
    ;     (make-vect 0.0 0.5))))

(define f1 (lambda (x) x))

(display (f1 2))

(define m (frame-coord-map f))

(paint einstein)

(einstein f); 'painter' is a procedure that takes 'frame' as argument, 'paint' gives the 
            ;argument to it and paint is on the console.


