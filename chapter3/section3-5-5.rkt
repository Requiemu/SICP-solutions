#lang racket

(random 5)

(pseudo-random-generator? (make-pseudo-random-generator))

(define r (make-pseudo-random-generator))

(current-pseudo-random-generator r)

(eq? r (current-pseudo-random-generator))

(vector->pseudo-random-generator! r #(1 2 3 4 5 6))

(random-seed 5)

(random 5 r)

(random 5)

(pseudo-random-generator->vector r)

(define rand
  (let ((x (random)))
    (lambda () 
      (set! x (random))
      x)))

(define random-numbers
  (stream-cons 
   (random)
   (stream-map (lambda (x) (random)) random-numbers)))

(define (stream-front-show stream n)
  (define (iter i)
    (if (>= i n)
        (begin (display (stream-ref stream i))
               (newline))
        (begin (display (stream-ref stream i))
               (newline)
               (iter (+ i 1)))))
  (iter 0))

(define rr (make-pseudo-random-generator))

(current-pseudo-random-generator rr)

(stream-front-show random-numbers 10)

(define rand-integer
  (let ((max (- (expt 2 32) 500)))
    (let ((x (random max)))
      (lambda ()
        (set! x (random max))
        x))))

(define random-integers
         (stream-cons (rand-integer)
                      (stream-map (lambda (x) (rand-integer)) random-integers)))

(stream-front-show random-integers 10)

(define (map-successive-pairs f stream)
  (stream-cons (f (stream-first stream) (stream-first (stream-rest stream)))
               (map-successive-pairs f (stream-rest (stream-rest stream)))))

(define cesaro-stream
  (map-successive-pairs (lambda (x y) (= (gcd x y) 1))
                        random-integers))

(stream-front-show cesaro-stream 10)

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (stream-cons
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-rest experiment-stream) passed failed)))
  (if (stream-first experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define p1
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0.000001 0)))

(stream-front-show p1 30)



          
          
          