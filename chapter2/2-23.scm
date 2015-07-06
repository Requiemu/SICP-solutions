(define (print-square l) 
  (for-each (lambda (x) (display (* x x))) l ))

(display (print-square '(1 2 3 4 5)))
