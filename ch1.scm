#lang racket
(require srfi/1) ; Required for iota which emulates python range.
(define (rng a b) (iota (- b a) a)) ; Return list of integers in interval [a, b).

; To run test code, uncomment the double semi-colons at the end of each section.

(define xs '(42 99 7)) ; for testing
(define ZeroToFive '(0 1 2 3 4 5))

(define (sq x) (* x x)) ; sq is an abbreviation for square 

; *******************************************
; Ex 1.3 [list of 3 numbers] -> [sum of squares of the two larger nums]
(define (ex1.3 xs)
  (apply + (map sq (remove (apply min xs) xs))))
;; (ex1.3 xs) ; '(42 99 7) -> 11565

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

(define (sqrt x) (sqrt-iter good-enough? 1.0 x))

(define (good-enough-rel? guess x)
  (let ([next-guess (improve guess x)])
    ; return true if |next-guess - guess|/guess < EPSILON
    (< (/ (abs (- next-guess guess)) guess) EPSILON)))

(define (sqrt-rel x) (sqrt-iter good-enough-rel? 1.0 x))
(define (sqrt-both x) (list (sqrt x) (sqrt-rel x)))
(define (sqrt-both-errors x)
  (let ([true-val (expt x 0.5)]  ; use built-in expt to get the true value
        [orig-val (sqrt x)]
        [rel-val  (sqrt-rel x)])
    (list (list 'error-original-version (- true-val orig-val))
          (list 'error-relative-version (- true-val rel-val)))))

(define SMALL_NUMBER_TO_TEST 0.01)
;; (sqrt-both-errors SMALL_NUMBER_TO_TEST) ; relative version gives an error that's smaller by several orders of magnitude

; *******************************************
; Ex 1.8: Newton's method for cube roots
(define (abs-diff x y) (abs (- x y))) ; absolute difference
(define (cube x) (expt x 3))
(define (true-cube-root x) (expt x 1/3))
; The error calculation wasn't mentioned in this exercise, but I thought it'd be interesting to see.
(define (error-cubert x) (abs-diff (true-cube-root x) (cubert x)))

(define (good-enough-cubert? guess x) 
  (< (abs (- (cube guess) x)) EPSILON))

(define (cubert-iter pred guess x) ; pred is predicate test to be used
  (if (pred guess x)
      guess
      (cubert-iter pred (improve-cubert guess x) x)))

(define (improve-cubert guess x)
  ; (x/guess^2 + 2*guess)/3
  (/ (+ (/ x (sq guess)) (* 2 guess)) 3))

(define (cubert x) (cubert-iter good-enough-cubert? 1.0 x))

;; (cube (cubert 2)) ; 2.0000592593226547
;; (cube (cubert 0.001)) ; 0.0013964147028898309 which is off by quite a bit

; *******************************************
; Ex 1.10: Ackermann's function
(define (A x y)
  (cond
    [(= y 0) 0]
    [(= x 0) (* 2 y)]
    [(= y 1) 2]
    [else (A (- x 1) (A x (- y 1)))]))

(println "Exercise 1.10, Ackermann values:")
(list (A 1 10) (A 2 4) (A 3 3))

(define (f n) (A 0 n)) ; f(n) = 2*n
(define (g n) (A 1 n)) ; g(n) = 2^n, where ^ is exponentiation
(define (h n) (A 2 n)) ; h(n) = 2^2^2...^2 n times, in other words, 2 to the 2 to the 2 ... n times
; so h(4) = 2^2^2^2 = 2^2^4 = 2^16 = 65536
; and h(5) = 2^2^2^2^2 = 2^h(4) = 2^65536 = "a number with over 19000 digits",
; or as Donald Trump would call it, "YUUUUUUUGE!!!". Since there are roughly 2^100 particles in the
; universe and h(5) is substantially larger than that, Mr. Trump is being his
; characteristically restrained self.

(define yuge (h 5)) ; Unfortunately this number is still to small to adequately
; fund Trump's wall (ahem - Wall, or better yet 'big, beautiful Wall') in either pesos or US dollars.
(define numberOfDigitsInYuge (length (string->list (number->string yuge))))
(println "numberOfDigitsInYuge:")
numberOfDigitsInYuge

; *******************************************
; Ex 1.11: recursive/iterative
(define (f-rec n) ; Recursive version is called f-rec
  (cond
    [(< n 3) n]
    [else (apply + (list (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3)))))]))

(define (f-iter n) ; Iterative version of f is f-iter
   (f-iter-h 0 1 2 n))

; f-iter helper function:
(define (f-iter-h fn-3 fn-2 fn-1 n) ; acc is accumulator and the fn values track the 3 most recent results
  (cond
    [(< n 2) n]
    [(= n 2) fn-1]
    [else (let ([f-next (+ fn-1 (* 2 fn-2) (* 3 fn-3))])
            (f-iter-h fn-2 fn-1 f-next (- n 1)))]))

; (time (f-iter 10000))
(define (time-proc/1 proc n)
  (time (proc n)))

(println "")
(println "Time of running Ex 1.10 f-rec on values in (rng 20 25):")
(map (lambda (n) (time-proc/1 f-rec n)) (rng 20 25))
(println "Time of running Ex 1.10 f-iter on n=1000:")
(time-proc/1 f-iter 1000)
; Note that although both procedures are implemented recursively,
; the iterative process runs orders of magnitude faster than the recursive
; process, but no surprise there, right?

; *******************************************
; *******************************************
; *******************************************
; *******************************************
; *******************************************

; *******************************************
; *******************************************

; *******************************************
; *******************************************
