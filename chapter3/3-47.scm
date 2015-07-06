;#lang racket
(load "parallel-execute.scm")
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define max-process 7)

(define (make-mutex)
  (let ((cell (list 0)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)));retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell (- (car cell) 1)))

(define (test-and-set! cell)
  (if (> (car cell) max-process)
      #t
      (begin (set-car! cell (+ (car cell) 1))
             #f)))

(define xitt (cons 10 'whatever))
(define s (make-serializer))
(parallel-execute (s (lambda () (set-car! xitt (* (car xitt) (car xitt)))))
                  (s (lambda () (set-car! xitt (* (car xitt) (car xitt) (car xitt)))))
                  ;(lambda () (set! x "hi"))
                  (lambda () (display xitt))
                  )
(newline)
(display xitt)
(newline)