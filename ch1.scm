#lang racket
(require srfi/1) ; Required for iota which emulates python range.
(define (rng a b) (iota (- b a) a)) ; Return list of integers in interval [a, b).

; To run test code, uncomment the double semi-colons at the end of each section, though not all sections have test cases
; and some have test cases I've left un-commented. It's a bit unorganized, but github is free so you get what you pay for.
; NB: These haven't been tested exhaustively so there could be errors.

(define xs '(42 99 7))  ; for testing
(define (sq x) (* x x)) ; sq is an abbreviation for square

; I know syntactic sugar causes cancer of the semicolon, but just like when you're
; at the Golden Corral chocolate fondue fountain, you'll be fine as long as you don't overindulge.
; So here's a few [sprinkles and gummy bears] *strikethrough* helper functions:
(define (-- x) (- x 1))
(define (++ x) (+ x 1))
(define (non-empty-list? xs) (and (list? xs) (> (length xs) 0)))
(define (singleton-list? xs) (and (non-empty-list? xs) (= (length xs) 1)))
(define (halve x) (/ x 2))
(define (double x) (* x 2))
(define (fst xs) (car xs))
(define (snd xs) (cadr xs))
(define (zero? x) (= x 0))
(define (iter f k n)
  (cond [(zero? k) n]
        [(= 1 k) (f n)]
        [else (f (iter f (-- k) n))]))

; *******************************************
; Ex 1.3 [list of 3 numbers] -> [sum of squares of the two larger nums]
(define (ex1.3 xs)
  (apply + (map sq (remove (apply min xs) xs))))
;; (ex1.3 '(42 99 7)) -> 11565

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
(println "Time of running Ex 1.10 f-rec on n = 20")
(time-proc/1 f-rec 20)
(println "Time of running Ex 1.10 f-iter on n=1000:")
(time-proc/1 f-iter 1000)
; Note that although both procedures are implemented recursively,
; the iterative process runs orders of magnitude faster than the recursive
; process, but no surprise there, right?

; *******************************************
; Ex 1.12, Pascal's triangle

; sum-between takes a list of n numbers and returns the list
; gotten by adding successive pairs, e.g. [a, b, c] -> [a+b, b+c]
(define (sum-between xs)
  (cond ; Assume  |xs| > 1, since summing between doesn't make sense otherwise
    [(= (length xs) 2) (list (apply + xs))]
    [else (cons (+ (car xs) (cadr xs)) (sum-between (cdr xs)))]))

; pt-nth-row gets nth row of Pascals triangle using 1 based indexing
(define pt-nth-row
  (lambda (n)
    (cond
      [(= n 1) '(1)]
      [(= n 2) '(1 1)]
      [else
       (let ([interior-list (sum-between (pt-nth-row (- n 1)))])
         (append '(1) interior-list '(1)))])))

(define (pascals-triangle n) ; first n rows of Pascal's triangle, 1 indexed
  (map pt-nth-row (rng 1 (++ n)))) ; ++n since the interval is half open, so n would be omitted otherwise

;; (pascals-triangle 10)
; *******************************************
; Ex 1.15 Skipped for now
; *******************************************
; Ex 1.16, exponentiation

; fast-expt from p 58:
(define (fast-expt b n)
  (cond [(= n 0) 1]
        [(even? n) (sq (fast-expt b (halve n)))]
        [else (* b (fast-expt b (-- n)))]))

; This is logarithmic in n since at every step it takes constant time for the trivial case,
; or it solves a subproblem for a new n which is either half of n or half of (n-1).
(define (fast-expt-iter-h b n a)
  (cond [(= n 0) a]
        [(even? n) (fast-expt-iter-h (sq b) (halve n) a)]
        [else (fast-expt-iter-h (sq b) (halve (-- n)) (* a b))]))

(define (fast-expt-iter b n)
  (fast-expt-iter-h b n 1))

; On my machine 2^100000 was computed in 0 ms, so realistically less than half a millisecond.
;; (time (fast-expt-iter 2 100000))
; *******************************************
; Ex 1.17, fast-mult
; This is the dual of 1.16, so just take that and replace * with + and 1 with 0.
(define (fast-mult-iter-h b n a)
  (cond [(= n 0) a]
        [(even? n) (fast-mult-iter-h (double b) (halve n) a)]
        [else (fast-mult-iter-h (double b) (halve (-- n)) (+ a b))]))

(define (fast-mult-iter b n)
  (fast-mult-iter-h b n 0))

; *******************************************
; Ex 1.18: 1.16 and 1.17 are already iterative, unless I'm missing something.
; *******************************************
; Ex 1.19: fibonacci log

; fib-iter from 1.2.2, p50:
#; (define (fib n) (fib-iter 1 0 n))
#; (define (fib-iter a b count)
  (if (= count 0) b (fib-iter (+ a b) a (-- count))))

; There's prolly a simpler way to do this, but this is what I got.
(define (T ab p q) ; ab is a list to facilitate iterating T(T(T...ab p q)..)
  (let ([a (fst ab)] [b (snd ab)])
    (list (+ (* (+ a b) q) (* a p)) (+ (* b p) (* a q)))))

; define T^2_xy for the 4 entries in the 2x2 matrix that is T^2
(define (Tsq_11 p q) (+ (sq (+ p q)) (sq q)))
(define (Tsq_12 p q) (+ (* 2 p q) (sq q)))
(define (Tsq_21 p q) (Tsq_12 p q)) ; T^2 is symmetric
(define (Tsq_22 p q) (+ (sq p) (sq q)))

(define (T^2 ab p q)
  (let ([a (fst ab)] [b (snd ab)])
    (list (+ (* (Tsq_11 p q) a) (* (Tsq_12 p q) b))
          (+ (* (Tsq_21 p q) a) (* (Tsq_22 p q) b)))))

(define f0 '(1 0)) ; start for fib
(define (T_01 ab) (T ab 0 1)) ; T_pq transform with [p, q] = [0, 1] for fibonacci 
(define (T^2_01 ab) (T^2 ab 0 1)) ; Ditto for T^2_01 transform
#; (define (T^k_01 ab k) ; iterate T_01 transform on [a,b] k times
  (let ([a (fst ab)] [b (snd ab)])
    (cond [(zero? k) ab]
          [(= k 1) (T_01 ab)]
          [(= k 2) (T^2_01 ab)]
          [(even? k)      (T^k_01 (T^2_01 ab) (halve     k))  ]
          [(odd? k) (T_01 (T^k_01 (T^2_01 ab) (halve (-- k))))])))

(define (T^k_01 ab k) ; iterate T_01 transform on [a,b] k times
 ; (let ([a (fst ab)] [b (snd ab)])
    (cond [(zero? k) ab]
          [(= k 1) (T_01 ab)]
          [(= k 2) (T^2_01 ab)]
          [(even? k)      (T^2_01 (T^k_01 ab (halve k)))]
          [(odd? k) (T_01 (T^2_01 (T^k_01 ab (halve (-- k)))))]))

(define (tk-simple ab k)
  (cond [(= 1 k) (t ab)]
        [(even? k) (iter t2 (halve k) ab)]
        [else (t (tk-simple ab (-- k)))]))

(define tk (lambda (k) (T^k_01 f0 k)))
(define t T_01)
(define (t-iter k) (iter t k f0))
(define (t2 ab) (T^2_01 ab))

(define (fib-log-pair n) (T^k_01 '(1 0) n))
;(define (fib-log n) (cadr (T^k_01 '(1 0) n)))
(define (fib-log n) (cadr (tk-simple '(1 0) n)))

#; (define (fib-iter ab n) ; using n instead of count b/c I'm too lazy to type cou [see what I did there?]
  (let ([a (fst ab)] [b (snd ab)])
    (cond [(zero? n) b]
          [(even? n) (fib-iter (T^2_01 ab) (halve n))]
          [else (T_01 (fib-iter (T^2_01 ab) (halve (-- n))))])))

(println "First 10 fibonacci's:")
(map fib-log (rng 1 11)) ; returns '(1 1 2 3 5 8 13 21 34 55)

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
; *******************************************
; *******************************************
; *******************************************
; *******************************************
; *******************************************
; *******************************************
; *******************************************
