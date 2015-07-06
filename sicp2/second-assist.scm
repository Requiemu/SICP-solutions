(define (filter f s)
  (define (iter origin result)
    (if (null? origin) result
      (if (f (car origin))
          (iter (cdr origin) (append result (list (car origin))))
          (iter (cdr origin) result))))
  (iter s '()))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (flat-map proc seq)
  (accumulate append null (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence) 
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (unique-pairs n)
  (flat-map 
    (lambda (i) 
      (map (lambda (j) (cons i j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 2 n)))

(display (unique-pairs 6))
(newline)

(define (sum-equal-s s)
  (if (odd? s)
      (map (lambda (x) (list (car x) (cdr x) (- s (+ (car x) (cdr x)))))
           (unique-pairs (/ (- s 1) 2)))
      (
