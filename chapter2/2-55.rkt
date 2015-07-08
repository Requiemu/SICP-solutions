#lang racket
(display (car ''abracadabra))
;because it is (car (quote (quote abracadabra)))