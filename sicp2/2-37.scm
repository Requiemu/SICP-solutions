(define (accumulate op initial sequence)
  (if (null? sequence) 
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define ma '((1 2 3 4) (4 5 6 6) (6 7 8 9)))

(define ve '(1 2 3 4))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (l) (dot-product l v)) m))

(display (matrix-*-vector ma ve))
(newline)

(define (transpose mat)
  (accumulate-n cons '() mat))

(display (transpose ma))
(newline)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mv) (matrix-*-vector cols mv)) m)))

(display (matrix-*-matrix ma (transpose ma)))

