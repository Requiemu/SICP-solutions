#lang planet neil/sicp
(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map car argstreams))
       (apply stream-map
              (cons proc (map cdr argstreams))))))
