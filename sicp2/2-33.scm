(define (accumulate op initial sequence)
  (if (null? sequence) 
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(display (accumulate + 0 (list 1 2 3 4 5)))
(newline)

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(display (map (lambda (x) (+ 1 x)) (list 1 2 3)))
(newline)

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(display (append (list 1 2 3) (list 4 5 6)))
(newline)

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(display (length (list 2 3 4 5 6 7)))
(newline)

