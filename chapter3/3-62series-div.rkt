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

(define (stream-sub a b)
  (stream-cons (- (stream-first a) (stream-first b))
               (stream-sub (stream-rest a) (stream-rest b))))

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

(define expo-05 (expo-series (/ 3.1415926525 4)))


;(display (exact->inexact (add-elements squa 100)))
;(newline)
;
;(display (exact->inexact (add-elements (stream-mul squa expo-05) 300)))

(define (invert-unit-series S)
  (define res (stream-cons 1 (stream-map (lambda (x) (- x))
                                         (series-mul (stream-rest S) res))))
  res)

(define x (invert-unit-series exp-series))

(display (stream-ref x 0))
(newline)

(define xx (stream-mul (series-mul x exp-series) expo-05))

(display (exact->inexact (add-elements xx 10)))



;(display (add-elements (stream-mul (series-mul x sine-series) expo-05) 2))

(define (series-div nu de)
  (define result (stream-cons (stream-first nu)
                              (stream-sub (stream-rest nu)
                                          (series-mul result (stream-rest de)))))
  (if (= (stream-first de) 0)
      (error "denominator can not be 0")
      (stream-scale result (/ 1 (stream-first de)))))

(define tangent (series-div sine-series cosine-series))

(newline)
(display (add-elements (stream-mul tangent expo-05) 100))
(newline)           

;(display (add-elements (stream-mul cosine-series expo-05) 1000))

;(define test (series-div ones exp-series))
;
;(display (stream-ref (series-mul test exp-series) 1))
;(newline)
;
;(display (stream-ref (series-mul test exp-series) 2))
;(newline)
;
;(display (stream-ref (series-mul test exp-series) 3))
;(newline)
;
;(display (stream-ref (series-mul test exp-series) 4))
;(newline)
;
;(display (stream-ref (series-mul test exp-series) 5))
;(newline)






