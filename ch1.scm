#lang racket

(define xs '(42 99 7)) ; for testing

(define (sq x) (* x x))

; Ex 1.3 [list of 3 numbers] -> [sum of squares of the two larger nums]
(define (ex1.3 xs)
  (apply + (map sq (remove (apply min xs) xs))))
(ex1.3 xs)