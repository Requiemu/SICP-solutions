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

(display (fold-right / 1 (list 1 2 3)))
(newline)

(display (fold-left / 1 (list 1 2 3)))
(newline)

(display (fold-right list null (list 1 2 3)))
(newline)

(display (fold-left list null (list 1 2 3)))
(newline)
