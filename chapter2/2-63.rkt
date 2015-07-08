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

(define a (make-tree 2 '() '()))

(define b (make-tree 4 '() '()))

(define c (make-tree 3 a b))

(tree->list-1 c)
(newline)

(tree->list-2 c)
(newline)

;tree in figure2.16

(define t1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))

(define t2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))

(define t3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))));this is not a standard binary tree defined in the book

(define t4 '(5 (1 () (11 () ())) (7 (3 () ()) (9 () ()))));this is not a standard binary tree defined in the book

(tree->list-1 t1)
(newline)

(tree->list-2 t1)
(newline)

(tree->list-1 t2)
(newline)

(tree->list-2 t2)
(newline)

(tree->list-1 t3)
(newline)

(tree->list-2 t3)
(newline)

(tree->list-1 t4)
(newline)

(tree->list-2 t4)
(newline)

;a.The two procedure has the same result.

;b.The two procedure has the same order of growth...?

        