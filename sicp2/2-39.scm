(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (accumulate op initial sequence)
  (if (null? sequence) 
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (fold-right a b c) (accumulate a b c))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) null sequence))

(display (reverse '(1 2 3 4)))
(newline)

(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) null sequence))

(display (reverse '(1 2 3 4)))
(newline)
