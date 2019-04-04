#lang racket

(define xs '(42 99 7)) ; for testing

(define (sq x) (* x x)) ; sq is an abbreviation for square 

; *******************************************
; Ex 1.3 [list of 3 numbers] -> [sum of squares of the two larger nums]
(define (ex1.3 xs)
  (apply + (map sq (remove (apply min xs) xs))))
;(ex1.3 xs) ; '(42 99 7) -> 11565

; *******************************************
; Ex 1.7 good-enough-rel? relative version
(define EPSILON 0.001) ; for checking if estimate of root is close enough

(define (sqrt-iter pred guess x) ; pred is predicate test to be used: good-enough? or good-enough-rel?
  (if (pred guess x)
      guess
      (sqrt-iter pred (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y) (/ (+ x y) 2))

; Original version of good-enough?
(define (good-enough? guess x) 
  (< (abs (- (sq guess) x)) EPSILON))

;
(define (sqrt x) (sqrt-iter good-enough? 1.0 x))

(define (good-enough-rel? guess x)
  (let ([next-guess (improve guess x)])
    ; return true if |next-guess - guess|/guess < EPSILON
    (< (/ (abs (- next-guess guess)) guess) EPSILON)))

(define (sqrt-rel x) (sqrt-iter good-enough-rel? 1.0 x))
(define (sqrt-both x) (list (sqrt x) (sqrt-rel x)))
(define (sqrt-both-errors x)
  (let ([true-val (expt x 0.5)]
        [orig-val (sqrt x)]
        [rel-val  (sqrt-rel x)]) ; use built-in expt to get the true value
    (list (list 'error-original-version (- true-val orig-val))
          (list 'error-relative-version (- true-val rel-val)))))

(define SMALL_NUMBER_TO_TEST 0.01)
;; (sqrt-both-errors SMALL_NUMBER_TO_TEST) ; relative version gives an error that's smaller by several orders of magnitude
; *******************************************
; *******************************************
; *******************************************
; *******************************************
; *******************************************
; *******************************************
; *******************************************
; *******************************************

; *******************************************
; *******************************************

; *******************************************
; *******************************************
