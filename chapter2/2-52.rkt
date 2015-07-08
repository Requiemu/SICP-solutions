#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;the "wave" in the book is too complicated, here I use a simpler image to represent 
;it.

(define v1 (make-vect 0 1))

(define v2 (make-vect 0.33 0))

(define v3 (make-vect 0.5 1))

(define v4 (make-vect 0.66 0))

(define v5 (make-vect 1 1))

(define wave
  (segments->painter
   (let ((m make-segment))
    (list (m v1 v2)
          (m v2 v3)
          (m v3 v4)
          (m v4 v5)))))

(paint wave)

(display "-----a-----")
(newline)

(define wave1
  (segments->painter
   (let ((m make-segment))
    (list (m v1 v2)
          (m v2 v3)
          (m v3 v4)
          (m v4 v5)
          (m (make-vect 0 0)
             (make-vect 1 1))))))

(paint wave1)

(display "-----b-----")
(newline)

(define (up-split1 painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split1 painter (- n 1))))
        (below painter (beside smaller diagonal-shading)))))

(define (right-split1 painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split1 painter (- n 1))))
        (beside painter (below smaller diagonal-shading)))))


(define (corner-split1 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split1 painter (- n 1)))
            (right (right-split1 painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split1 painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(paint (corner-split1 einstein 3))

(display "-----c-----")
(newline)

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
