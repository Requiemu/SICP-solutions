(define (square x) (* x x))

(define (square-tree l)
  (map (lambda (subtree)
               (if (pair? subtree)
                   (square-tree subtree)
                   (square subtree)))
       l))

(define l0 (list 1
                 (list 2 (list 3 4 5)
                 (list 6 7))))
           
(display (square-tree l0))
