#lang planet neil/sicp

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (empty-queue?) 
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue" )
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((pair (cons item '())))
        (cond ((null? front-ptr)
               (begin (set-front-ptr! pair)
                      (set-rear-ptr! pair)
                      front-ptr))
              (else (begin (set-cdr! rear-ptr pair)
                           (set-rear-ptr! pair)
                           front-ptr)))))
    (define (delete-queue!) 
      (if (empty-queue?) 
          (error "DELETE-QUEUE! called with a empty queue" )
          (begin (set-front-ptr! (cdr front-ptr))
                 front-ptr)))
    (define (display-queue) front-ptr)
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'display-queue) display-queue)
            (else (error "DISPATCH--invalid request" m))))
    dispatch))

(define (empty-queue? queue) ((queue 'empty-queue?)))

(define (front-queue queue) ((queue 'front-queue)))

(define (insert-queue! queue item) ((queue 'insert-queue!) item))

(define (delete-queue! queue) ((queue 'delete-queue!)))

(define (display-queue queue) ((queue 'display-queue)))
;---------------------------queue-----------------------------------------

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
  
  ;(define m (make-wire))
  
  ;(get-signal m)
  ;
  ;(set-signal! m 1)
  ;
  ;(get-signal m)
  
  ;(define add-action! wire

;----------------------------wire-----------------------------------------

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agneda) (car agneda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))
              
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define the-agenda (make-agenda))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))



;--------------------agenda-----------------------------------------------

  
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
  
  (define (half-adder a b s c)
    (let ((d (make-wire)) (e (make-wire)))
      (or-gate a b d)
      (and-gate abc)
      (inverter c e)
      (and-gate d e s)
      'ok))
  
  (define (full-adder a b c-in sum c-out)
    (let ((s (make-wire))
          (c1 (make-wire))
          (c2 (make-wire)))
      (halg-adder b c-in s c1)
      (half-adder a s sum c2)
      (or-gate c1 c2 c-out)
      'ok))
  
  (define (ripple-carry-adder a b s c)
    (let ((c-in (make-wire)))
      (if (null? (cdr a))
          (set-signal! c-in 0)
          (ripple-carry-adder (cdr a) (cdr b) (cdr s) c-in))
      (full-adder (car a) (car b) c-in (car s) c)))

;;--------------------------------box---------------------------------
  