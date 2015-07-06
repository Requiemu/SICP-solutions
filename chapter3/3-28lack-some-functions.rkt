#lang planet neil/sicp

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (get-signal) signal-value)
    
    (define (set-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
      
      (define (dispatch m)
        (cond ((eq? m 'get-signal) get-signal)
              ((eq? m 'set-signal!) set-signal!)
              ((eq? m 'add-action!) accept-action-procedure!)
              (else (error "Unknow operation" m))))
      dispatch))
  
  (define (get-signal wire) ((wire 'get-signal)))
  
  (define (set-signal! wire new-value) ((wire 'set-signal!) new-value))

(define (add-action! wire procedure-of-no-arguments)
  (wire 'add-action!) procedure-of-no-arguments)

;(get-signal m)
;
;(set-signal! m 1)
;
;(get-signal m)

;(define add-action! wire

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () 
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and a1 a2)
  (cond ((and (= a1 0) (= a2 0)) 0)
        ((and (= a1 0) (= a2 1)) 0)
        ((and (= a1 1) (= a2 0)) 0)
        ((and (= a1 1) (= a2 1)) 1)
        (else (error "Invalid signal--LOGICAL-AND" a1 a2))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or a1 a2)
    (cond ((and (= a1 0) (= a2 0)) 0)
        ((and (= a1 0) (= a2 1)) 1)
        ((and (= a1 1) (= a2 0)) 1)
        ((and (= a1 1) (= a2 1)) 1)
        (else (error "Invalid signal--LOGICAL-OR" a1 a2))))
         