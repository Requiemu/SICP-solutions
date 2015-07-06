(newline)
(display "----------a----------")
(newline)

(define (make-mobile left right)
  (list left right))

(define (left-branch mobile) (list-ref mobile 0))

(define (right-branch mobile) (list-ref mobile 1))

(define (make-branch length1 structure)
  (list length1 structure))

(define (branch-length branch) (list-ref branch 0))

(define (branch-structure branch) (list-ref branch 1))

(newline)
(display "----------b----------")
(newline)

(define (total-weight mobile)
  (if (number? mobile)
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))

(define l (make-branch 2 3))

(define r (make-branch 3 2))

(define m (make-mobile l r))
 
(display (total-weight m))
(newline)

(newline)
(display "----------c----------")
(newline)

(define (balance? m) 
 (if (number? m) #t
  (and (= (* (total-weight (branch-structure (left-branch m)))
             (branch-length (left-branch m)))
          (* (total-weight (branch-structure (right-branch m)))
             (branch-length (right-branch m))))
       (balance? (branch-structure (left-branch m)))
       (balance? (branch-structure (right-branch m))))))

(display (balance? m))
(newline)

(define m1 (make-mobile  (make-branch 5 1) (make-branch 1 m)))

(display (balance? m1))
(newline)

(define m2 (make-mobile  (make-branch 5 1) (make-branch 2 2.49)))

(display (balance? m2))
(newline)


(newline)
(display "----------d----------")
(newline)

;just need to modify the element's fetching method:

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch m) (car m))

(define (right-branch m) (cdr m))

(define (branch-length b) (car b))

(define (branch-structure b) (cdr b))

(define l (make-branch 2 3))

(define r (make-branch 3 2))

(define m (make-mobile l r))
 
(display (total-weight m))
(newline)

(display (balance? m))
(newline)

(define m1 (make-mobile  (make-branch 5 1) (make-branch 1 m)))

(display (balance? m1))
(newline)

(define m2 (make-mobile  (make-branch 5 1) (make-branch 2 2.49)))

(display (balance? m2))
(newline)

