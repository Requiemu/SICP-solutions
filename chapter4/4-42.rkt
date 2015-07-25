#lang racket


(define (solution)
  (let ((Betty (amb 1 2 3 4 5))
        (Ethel (amb 1 2 3 4 5))
        (Joan  (amb 1 2 3 4 5))
        (Kitty (amb 1 2 3 4 5))
        (Maty  (amb 1 2 3 4 5)))
    (require 
      (distinct? (list Betty Ethel Joan Kitty Maty)))
    (require (xor (= Kitty 2) (= Betty 3)))
    (require (xor (= Ethel 1) (= Joan 2)))
    (require (xor (= Joan 3) (= Ethel 5)))
    (require (xor (= Kitty 2) (= Mary 4)))
    (require (xor (= Mary 4) (= Betty 1)))
    (list Betty Ethel Joan Kitty Mary)))
  
  