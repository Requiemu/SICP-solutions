#lang racket
(display (list 'a 'b 'c))
(newline)

(display '(a b c))
(newline)

(display '(a b c 1))
(newline)
(display '1)
(display 1)
(newline)

(define a '(a b c 1))
(display (cadddr a))
(newline)
(display (symbol? (cadddr a)))
(newline)
(display (number? (cadddr a)));test the use of '
(newline)

(display (list 'george))
(newline)
(display (list (list 'george)))
(newline)

(display (cdr '((x1 x2) (y1 y2))))
(newline)

(display (pair? (car '(a short list))))
(newline)

(display (memq 'red '((red shoes) (blue socks))))
(newline)
(display (memq 'red '(red shoes blue socks)))
(newline)


         
