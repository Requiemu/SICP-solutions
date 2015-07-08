#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (below1 painter1 painter2)
  (let ((painter-down
         ((transform-painter
           (make-vect 0 0)
           (make-vect 1 0)
           (make-vect 0 0.5))
         painter1))
        (painter-up
         ((transform-painter
           (make-vect 0 0.5)
           (make-vect 1 0.5)
           (make-vect 0 1))
          painter2)))
    (lambda (frame)
      (painter-down frame)
      (painter-up frame))))

(paint einstein)

(paint (below1 einstein einstein))

(paint (below1 (below1 einstein einstein) (below1 einstein einstein)))

(define (below2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

(paint (below2 einstein einstein))
  
        