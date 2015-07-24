#lang racket

(define (list-set! l n m)
  (define (iter origin result k)
    (if (null? origin) 
        result
        (if (= k n) 
            (iter (cdr origin) (append result (list m)) (+ k 1))
            (iter (cdr origin) (append result (list (car origin))) (+ k 1)))))
  (iter l '() 0))

(define (flat-map map-func l)
  (define raw (map map-func l))
  (define (iter l result)
    (if (null? l) result
        (iter (cdr l) (append (car l) result))))
  (iter raw '()))


(define (building n)
  (define (iter i)
    (if (> i n) '()
        (cons (list #f i) (iter (+ i 1)))))
  (iter 1))

(define (total-floor building)
  (define (iter rest-floor)
    (if (null? (cdr rest-floor)) (cadar building)
        (iter (cdr rest-floor))))
  (iter building))

(define the-empty-apartment (building 5))

;'((#f 1)
;  (#f 2)
;  (#f 3)
;  (#f 4)
;  (#f 5))

(define name '(b c f m s))

(define (floor n building)
  (define (iter rest-floor num)
    (if (null? rest-floor) #f
        (if (eq? n (caar rest-floor)) num
            (iter (cdr rest-floor) (+ num 1)))))
  (iter building (total-floor building)))

;(floor 'a the-apartment)

(define (distinct? l)
  (define (different? i lj)
    (if (or (null? lj)
            (eq? i #f))
        #t
        (if (eq? i (car lj)) #f
            (different? i (cdr lj)))))
  (define (iter i j)
    (if (null? j) #t
        (if (different? (car j) i)
            (iter (cons (car j) i) (cdr j))
            #f)))
  (iter '() l))

;(distinct? '(1 2 #f #f 3))

(define (list-all-true? l)
  (if (null? l) #t 
      (if (car l) (list-all-true? (cdr l))
          #f)))

(define (final-check a-building names)
  (define (f n) (floor n a-building))
  (if (> (length a-building) (length names))
      (list-all-true? (map f names))
      (list-all-true? (map car a-building))))

(define (safe? a-building names)
  (define (f n) (floor n a-building))
  (and (distinct? 
        (map car a-building)) 
       (distinct? 
        (map 
         f
         '(b c f m s)))
       (not (eq? (f 'b) 5))
       (not (eq? (f 'c) 1))
       (not (eq? (f 'f) 5))
       (not (eq? (f 'f) 1))
       (if (and (f 'm) (f 'c)) 
           (> (f 'm) (f 'c)) 
           #t)
       (if (and (f 's) (f 'f)) 
           (not (= (abs (- (f 's) (f 'f))) 1))
           #t)
       (if (and (f 'f) (f 'c))
           (not (= (abs (- (f 'f) (f 'c))) 1))
           #t)))

(define (solution num names)
  (define (solution-col n)
    (if (= n 0) (list (building num))
        (filter (lambda (x) (safe? x names))
                (flat-map (lambda (origin)
                            (map (lambda (a-name) (list-set! 
                                                   origin 
                                                   (- n 1)
                                                   (cons a-name
                                                         (cdr (list-ref origin (- n 1))))))
                                 (cons #f names)))
                          (solution-col (- n 1))))))
  (filter (lambda (bb) (final-check bb names)) (solution-col num)))

(define (display-list l)
  (define (iter li)
    (if (null? (cdr li)) 
        (begin
          (display (car li))
          (newline))
        (begin 
          (display (car li))
          (newline)
          (iter (cdr li)))))
  (iter l))

(display-list (solution 5 name))

(display-list (solution 6 name))




























