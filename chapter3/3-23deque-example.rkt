#lang planet neil/sicp

;the list model can not satisfy the requirement, need to use the linked list.

(define (make-deque)
  (cons '() '()))

(define (front-ptr deque) (car deque))

(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item) (set-car! deque item))

(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) 
  (null? (front-ptr deque)))

(define (front-deque deque)
  (caar (front-ptr deque)))

(define (rear-deque deque)
  (caar (rear-ptr deque)))

(define (front-insert-deque! deque item)
  (let ((pair (cons (cons item '()) '())))
    (cond ((empty-deque? deque) (begin (set-front-ptr! deque pair)
                                       (set-rear-ptr! deque pair)
                                       ))
          (else (begin (set-cdr! pair (front-ptr deque))
                       (set-cdr! (car (front-ptr deque)) pair)
                       (set-front-ptr! deque pair)
                       )))))

(define (rear-insert-deque! deque item)
  (let ((pair (cons (cons item '()) '())))
    (cond ((empty-deque? deque) (begin (set-front-ptr! deque pair)
                                       (set-rear-ptr! deque pair)
                                       ))
          (else (begin (set! pair (cons (cons item (rear-ptr deque)) '()))
                       (set-cdr! (rear-ptr deque) pair)
                      (set-rear-ptr! deque pair))))))




(define (front-delete-deque! deque)
  (if (null? (front-ptr deque))
      (error ("FRONT-DELETE-DEQUE!--the deque is empty" deque))
      (begin (set-front-ptr! deque (cdr (front-ptr deque)))
             (set-cdr! (car (front-ptr deque)) '())
             )))

(define (rear-delete-deque! deque)
  (if (null? (front-ptr deque))
      (error ("REAR-DELETE-DEQUE! -- the deque is empty" deque))
      (begin (set-rear-ptr! deque (cdar (rear-ptr deque)))
             (set-cdr! (rear-ptr deque) '())
             )))

(define (display-deque deque)
  (define (iter ptr result)
    (if (null? ptr) (reverse result)
        (iter (cdr ptr) (cons (caar ptr) result))))
  (iter (front-ptr deque) '()))

(define a (make-deque))

(front-insert-deque! a 'a)

(display (display-deque a))
(newline)

(rear-insert-deque! a 'b)

(display (display-deque a))
(newline)

(rear-insert-deque! a 'c)

(display (display-deque a))
(newline)

(front-insert-deque! a 1)
(display (display-deque a))
(newline)

(front-delete-deque! a)
(display (display-deque a))
(newline)

(rear-delete-deque! a)
(display (display-deque a))
(newline)

(front-deque a)

