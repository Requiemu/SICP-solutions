#lang racket
 ( require ( planet "sicp.ss" ( "soegaard" "sicp.plt" 2 1)))

(paint einstein)

(paint (flip-vert einstein))

(paint (flip-horiz einstein))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(paint (right-split einstein 2))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;(define (corner-split painter n)
;  (if (= n 0) painter
;      (beside (below painter painter)
;              (below (corner-split painter (- n 1)) painter))))

;(paint (corner-split einstein 3))

;(define (center painter n)
;  (let ((k (corner-split painter n)))
;    (beside (below (flip-vert k) k)
;            (below (flip-horiz (flip-vert k)) (flip-horiz k)))))

;(paint (center einstein 10))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(paint (corner-split einstein 3))

(define (center painter n)
  (if (= n 0)
      painter
      (let ((k (corner-split painter n)))
        (beside (below k (flip-vert k))
                (below (flip-horiz k) (flip-horiz (flip-vert k)))))))

(paint (center einstein 3))
          
          