#lang racket
;根据字母及其权重生成haffman树，本次所实现的方法能够根据并没有顺序排列的字母权重对生成haffman树。
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree) (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
                       
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)   ;symbol
                               (cadr pair)) ;frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

;2-67 test

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define sample-symbols (decode sample-message sample-tree))

;2-68

(define (element-of-set x set)
  (cond ((eq? '() set) #f)
        ((equal? x (car set)) #t)
        ((not(equal? x (car set))) (element-of-set x (cdr set)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((and (element-of-set symbol (symbols (left-branch tree)))  
             (leaf? (left-branch tree)))
         (list 0))
        ((and (element-of-set symbol (symbols (left-branch tree)))
              (not (leaf? (left-branch tree))))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((and (element-of-set symbol (symbols (right-branch tree)))  
             (leaf? (right-branch tree)))
         (list 1))
        ((and (element-of-set symbol (symbols (right-branch tree)))
              (not (leaf? (right-branch tree))))
         (cons 1 (encode-symbol symbol (right-branch tree))))))

(encode sample-symbols sample-tree)
               
;2-69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (smallest-weight-element set)
  (cond ((null? (cdr set)) (car set))
        ((>= (weight (car set)) (weight (cadr set))) (smallest-weight-element (cdr set)))
        ((< (weight (car set)) (weight (cadr set))) (smallest-weight-element (cons (car set)
                                                                                   (cddr set))))))


  
  

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set)) leaf-set
      (let ((min-1 (smallest-weight-element leaf-set)))
          (let ((min-2 (smallest-weight-element (remove min-1 leaf-set))))
            (let ((rest-set (remove min-2 (remove min-1 leaf-set))))
              (successive-merge (cons (make-code-tree min-1 min-2) rest-set)))))))

(define sample-pairs '((A 4) (B 2) (C 1) (D 1)))

(generate-huffman-tree sample-pairs)
              
  



