#lang racket

(require racket/stream)

(define (stream-add a b)
  (stream-cons (+ (stream-first a) (stream-first b))
               (stream-add (stream-rest a) (stream-rest b))))

(define (stream-div s1 s2)
  (stream-cons (/ (stream-first s1)
                  (stream-first s2))
               (stream-div (stream-rest s1)
                           (stream-rest s2))))

(define (stream-mul s1 s2)
  (stream-cons (* (stream-first s1)
                  (stream-first s2))
               (stream-mul (stream-rest s1)
                           (stream-rest s2))))

(define (stream-scale stream factor)
  (stream-map (lambda (x) (* x factor)) stream))


(define sense-data 
  (stream-cons (- (random 10) 5)
               (stream-map 
               (lambda (x) (- (random 10) 5))
               sense-data)))
               

(define (sign-change-detector x y)
  (if (< (* x y) 0) 1
      0))

(define (smooth s)
  (stream-cons (stream-first s)
               (stream-scale (stream-add s (stream-rest s)) 0.5)))


(define (make-zero-crossings input-stream last-value)
  (stream-cons
   (sign-change-detector 
    (stream-first input-stream) last-value)
   (make-zero-crossings (stream-rest input-stream)
                        (stream-first input-stream))))

(define zero-crossings 
  (make-zero-crossings (smooth sense-data) 0))




;(define (stream-map-boss proc . argstreams)
;  (if (null? (car argstreams))
;      empty-stream
;      (stream-cons
;       (apply proc (map stream-first argstreams))
;       (apply stream-map-boss
;              (cons proc (map stream-rest argstreams))))))
;
;(define zero-crossings-boss
;  (stream-map-boss sign-change-detector
;                   sense-data
;                   (stream-cons 0
;                                sense-data)))

(display (stream-ref zero-crossings 0)) (display "  ")(display (stream-ref sense-data 0))
(newline)

(display (stream-ref zero-crossings 1)) (display "  ")(display (stream-ref sense-data 1))
(newline)

(display (stream-ref zero-crossings 2)) (display "  ")(display (stream-ref sense-data 2))
(newline)

(display (stream-ref zero-crossings 3)) (display "  ")(display (stream-ref sense-data 3))
(newline)

(display (stream-ref zero-crossings 4)) (display "  ")(display (stream-ref sense-data 4))
(newline)

(display (stream-ref zero-crossings 5)) (display "  ")(display (stream-ref sense-data 5))
(newline)

(display (stream-ref zero-crossings 6)) (display "  ")(display (stream-ref sense-data 6)) 
(newline)

(display (stream-ref zero-crossings 7)) (display "  ")(display (stream-ref sense-data 7)) 
(newline)

(display (stream-ref zero-crossings 8)) (display "  ")(display (stream-ref sense-data 8))
(newline)

(display (stream-ref zero-crossings 9)) (display "  ")(display (stream-ref sense-data 9))
(newline)


;(display "---------boss-------------")
;(newline)
;
;(display (stream-ref zero-crossings-boss 0)) (display "  ")(display (stream-ref sense-data 0))
;(newline)
;
;(display (stream-ref zero-crossings-boss 1)) (display "  ")(display (stream-ref sense-data 1))
;(newline)
;
;(display (stream-ref zero-crossings-boss 2)) (display "  ")(display (stream-ref sense-data 2))
;(newline)
;
;(display (stream-ref zero-crossings-boss 3)) (display "  ")(display (stream-ref sense-data 3))
;(newline)
;
;(display (stream-ref zero-crossings-boss 4)) (display "  ")(display (stream-ref sense-data 4))
;(newline)
;
;(display (stream-ref zero-crossings-boss 5)) (display "  ")(display (stream-ref sense-data 5))
;(newline)
;
;(display (stream-ref zero-crossings-boss 6)) (display "  ")(display (stream-ref sense-data 6)) 
;(newline)
;
;(display (stream-ref zero-crossings-boss 7)) (display "  ")(display (stream-ref sense-data 7)) 
;(newline)
;
;(display (stream-ref zero-crossings-boss 8)) (display "  ")(display (stream-ref sense-data 8))
;(newline)
;
;(display (stream-ref zero-crossings-boss 9)) (display "  ")(display (stream-ref sense-data 9))
;(newline)