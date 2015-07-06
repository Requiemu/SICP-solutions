#lang racket
(require racket/stream)

(define (stream-display s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (integers-from n)
  (stream-cons n (integers-from (+ n 1))))

(define integers (integers-from 1))

(define ones (stream-cons 1 ones))

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

(define (integrate-series s)
  (stream-div s integers))

(define integ-ones (integrate-series ones))

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))



(define cosine-series
  (stream-cons 1 (stream-scale (integrate-series sine-series) -1)))

(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

(define (series-mul s1 s2)
  (stream-cons (* (stream-first s1) (stream-first s2))
               (stream-add (stream-add (stream-scale (stream-rest s1) (stream-first s2))
                                       (stream-scale (stream-rest s2) (stream-first s1)))
                           (stream-cons 0 (series-mul (stream-rest s1)
                                                      (stream-rest s2))))))

(define squa (stream-add (series-mul sine-series sine-series) (series-mul cosine-series cosine-series)))

(display (stream-ref squa 1))
(newline)

(display (stream-ref squa 2))
(newline)

(display (stream-ref squa 3))
(newline)

(display (stream-ref squa 4))
(newline)

(define (add-elements s1 maxn)
  (define (iter s n result)
    (if (> n maxn) result
        (iter (stream-rest s) (+ n 1) (+ result (stream-first s)))))
  (iter s1 0 0))

(define (expo-series x)
  (define result (stream-cons x (stream-scale result x)))
  (stream-cons 1 result))

(define expo-05 (expo-series 0.5))


(display (exact->inexact (add-elements squa 100)))
(newline)

(display (exact->inexact (add-elements (stream-mul squa expo-05) 300)))





