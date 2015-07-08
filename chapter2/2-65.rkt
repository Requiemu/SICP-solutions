#lang racket
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        (= x (entry set) #t)
        (< x (entry set) (element-of-set? x (left-branch set)))
        (> x (entry set) (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) 
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (list (entry tree))
              (tree->list-1 (right-branch tree)))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
         (copy-to-list (left-branch tree)
                       (cons (entry tree)
                             (copy-to-list (right-branch tree)
                                           result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
;in order to form a balanced tree, the input list must be inincrease order.

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


(define (intersection-set tree1 tree2)
  (let ((set1 (tree->list-1 tree1)) 
        (set2 (tree->list-1 tree2)))
    (list->tree (intersection-set-list set1 set2))))

(define (union-set tree1 tree2)
  (let ((set1 (tree->list-1 tree1))
        (set2 (tree->list-1 tree2)))
    (list->tree (union-set-list set1 set2))))

    
    
    
(define (intersection-set-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set-list (cdr set1)
                                            (cdr set2))))
             ((< x1 x2)
              (intersection-set-list (cdr set1) set2))
             ((< x2 x1)
              (intersection-set-list set1 (cdr set2)))))))


(define (union-set-list set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1
                        (union-set-list (cdr set1)
                                    (cdr set2))))
                 ((< x1 x2)
                  (cons x1
                        (union-set-list (cdr set1)
                                    set2)))
                 ((< x2 x1)
                  (cons x2
                        (union-set-list set1
                                    (cdr set2)))))))))

(define t1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))

(define t2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))

(define t3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))));this is not a standard binary tree defined in the book

(define t4 '(5 (1 () (11 () ())) (7 (3 () ()) (9 () ()))));this is not a standard binary tree defined in the book

(define t5 '(6 (2 () (4 () ())) (10 (8 () ()) (12 () ()))))

(define t6 '(6 (2 (1 () ()) (4 () (5 () ()))) (10 (8 () ()) (12 () ()))))

(union-set t1 t2)
(newline)

(union-set t1 t5)
(newline)

(intersection-set t1 t5)
(newline)

(intersection-set t1 t6)
(newline)