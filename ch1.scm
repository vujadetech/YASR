#lang racket
(require srfi/1) ; Required for iota which emulates python range.

; Didn't realize range was already a builtin, but leaving it in (and commented out) for now.
#; (define range ; (iota count [start step]) == range([start=0], stop, [step]) ; python3 range
  (case-lambda
    [(start stop step)
     (let ([count (ceiling (/ (- stop start) step))])
       (iota count start step))]
    [(start stop) (range start stop 1)] ; Return list of integers in interval  [start, stop).
    [(stop) (range 0 stop)])) ; Return list of integers in interval [0, stop).

; To run test code, uncomment the double semi-colons at the end of each section, though not all sections have test cases
; and some have test cases I've left un-commented. It's a bit unorganized, but github is free so you get what you pay for.
; NB: These haven't been tested exhaustively so there could be errors.

(define xs '(42 99 7))    ; for testing
(define (square x) (* x x))
(define sq square) ; sq is an abbreviation of square for Mr. Lazybone's over here, i.e., "Yours truly"

; Some helper functions:
(define mod remainder)
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
(define (time-ms proc . xs) ; time in ms of running proc on xs, return pair (result . "time in ms") 
    (let* ([t0 (current-inexact-milliseconds)] [y (apply proc xs)] [delta (- (current-inexact-milliseconds) t0)])
        (cons y delta)))
(define (first-two-same? x . xs)
  (if (= x (car xs)) #t #f))
(define (mean xs) (/ (apply + xs) (length xs))) ; arithmetic mean
(define (decades n) (map (lambda (n) (expt 10 n)) (range 0 n))) ; useful for checking O(f(n)) growth, e.g. in prime testing problems

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

;;(println "Exercise 1.10, Ackermann values:")
;;(list (A 1 10) (A 2 4) (A 3 3))

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
;;(define numberOfDigitsInYuge (length (string->list (number->string yuge))))
;;(println "numberOfDigitsInYuge:")
;;numberOfDigitsInYuge

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

;;(println "")
;;(println "Time of running Ex 1.10 f-rec on n = 20")
;;(time-proc/1 f-rec 20)
;;(println "Time of running Ex 1.10 f-iter on n=1000:")
;;(time-proc/1 f-iter 1000)
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
  (map pt-nth-row (range 1 (++ n)))) ; ++n since the interval is half open, so n would be omitted otherwise

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

(define (tn-simple ab n) ; modelled after fast-exp
  (cond [(zero? n) ab]
        [(even? n) (tn-simple (t2 ab) (halve n))]
        [else (t (tn-simple ab (-- n)))]))

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
(map fib-log (range 1 11)) ; returns '(1 1 2 3 5 8 13 21 34 55)
;;; TODO: verify that my version is O(log n), and do the textbook version on p62.
; *******************************************
; Procs given on pp67-68:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))        

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; *******************************************
; Ex 1.21, smallest-divisor

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

; We can test whether a number is prime as follows: n is prime if and only if n is its own smallest divisor.

(define (prime? n) (= n (smallest-divisor n)))

;;; (map smallest-divisor '(199 1999 19999)) ; First two are prime, last one has 7 as a factor.
; *******************************************
; Ex 1.22, Search for primes
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n))

(define (start-prime-test n [start-time (current-inexact-milliseconds)]) 
  (when (prime? n)
      (report-prime (- (current-inexact-milliseconds) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (oddify n) ; returns n if n is already odd, otherwise adds 1 to make it odd
  (if (odd? n) n (++ n)))

(define (odds-in-range a b) ; a can be even and range is half open so b is omitted regardless
  (range (oddify a) b 2))

(define (search-for-primes a b) ; search for primes among the odd values in [a, b)
  (filter prime? (odds-in-range a b)))

(define (pi_ N) (/ N (log N)))  ; pi_(N) function given by the prime number theorem

(define (find-k-smallest-primes>n k n) ; find the k smallest primes that are larger than n
  (let ([a n] [b (+ n (* k (ceiling (pi_ n))))]) ; use interval [a, a + k*pi_(n)) to test for primes, which
    ; may not work since prime number theorem is statistical
    (let* ([candidates (search-for-primes a b)] ; candidates b/c there's no guarantee k will be present
           [j (length candidates)])   
      (if (>= j k) ; enough primes were generated ...
          (take candidates k) ; ... so return the k primes
          (append candidates (find-k-smallest-primes>n (- k j) (last candidates))))))) ; didn't get enough, so 
          ; keep the j candidates found so far and add k-j more recursively, setting n = last candidate found

(define kspn find-k-smallest-primes>n) ; abbreviation

; This is a more general version of root-n-pred below, but not using it yet since prolly overkill for this.
#; (define (predicted x1 y1 x2 proc) ; x1y1 is a pair, x2 an x-coord, return pair (x2 . y2)
  ; where y2 the result of applying proc to x1 and y1 (e.g., root(n) growth)
  (cons x2 (proc x1 y1 x2)))

(define (root-n-pred x1 y1 x2) ; get y2 from x1 y1 and x2 given root(n) growth
  (let* ([n (/ x2 x1)] [scale (sqrt n)]) (* y1 scale)))

(define (prime-times) ; ps is a list of primes, returns list of times for testing their primality in ms
; This fall on CBS after Survivor (b/c it's "prime-times", get it?).
; Apropos that, this code could use a refactoring as it's currently a bigger trainwreck than Phillip from a season of Survivor a few years ago.
  (let* ([k 3] [primes1000 (kspn k 1000   )]
               [primes10k  (kspn k 10000  )]
               [primes1mil (kspn k 1000000)]
               )
    (let ([times1000 (map cdr (map (lambda (p) (time-ms prime? p)) primes1000))]
          [times10k (map cdr (map (lambda (p) (time-ms prime? p)) primes10k))]
          [times1mil (map cdr (map (lambda (p) (time-ms prime? p)) primes1mil))]
          )
      (let ([real-times (list (list 1000 (mean times1000))
                              (list 10000 (mean times10k))
                              (list 1000000 (mean times1mil))
                              )])
        (display "Real times: ") (displayln real-times)
        
        (define expected-times
          (let* ([nt0 (car real-times)] [n0 (car nt0)] [t0 (cadr nt0)])
            (zip (map car real-times)
            (cons t0 (map (lambda (n1) (root-n-pred n0 t0 n1)) (map car (cdr real-times)))))))
        (display "Expected times: ") (displayln expected-times)
        ))))

;; (prime-times)
; My machine gave this output:
; Real times: ((1000 0.004964192708333333) (10000 0.013346354166666666) (1000000 0.1142578125))
; Expected times: ((1000 0.004964192708333333) (10000 0.01569815572719044) (1000000 0.1569815860589814))
; So for these cases the expected is diverging above the real value with the
; difference increasing as n gets larger.

; End Ex 1.22
; *******************************************
; Ex 1.23, smallest-divisor+ ; upgrade of smallest-divisor by omitting redundant test of evens > 2.

; (find-divisor+ n test-divisor next-odd)
(define (next-odd n) (+ n 1 (mod n 2))) ; add an extra 1 if n is odd (i.e., n (mod 2) = 1)

(define (find-divisor+ n test-divisor) ; refactor of find-divisor; '+' b/c it will be an improvement, or at least one hopes.
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor+ n (next-odd test-divisor)))))

(define (smallest-divisor+ n) ; refactor of smallest-divisor
  (find-divisor+ n 2))

(define (prime?+ n) (= (smallest-divisor+ n) n)) ; improved prime? proc

(define (timed-prime-test+ n prime-proc?) ; More generic versions of the test functions which allow specifying prime-proc?
  (newline)
  (display n)
  (start-prime-test+ n prime-proc?))

(define (start-prime-test+ n prime-proc? [start-time (current-inexact-milliseconds)]) 
  (when (prime-proc? n)
      (report-prime (- (current-inexact-milliseconds) start-time))))

(define (ex1.23 k n) ; run time tests on k primes larger than n
  (let ([xs (kspn k n)]) ; Recall kspn is abbrev. for find-k-smallest-primes>n
    (display "Prime? times")
    (map (λ (n) (timed-prime-test+ n prime?)) xs)
    (display "\nPrime?+ times")
    (map (λ (n) (timed-prime-test+ n prime?+)) xs)
    (void)))

(define tpt timed-prime-test) ; abbreviations for testing
(define tpt+ timed-prime-test+)
;; (ex1.23 3 10000) ; My machine gave a speed improvement with the prime?+ version,
; but not quite twice as fast, as was hoped. Occasionally it was slower which was weird. 

; *******************************************
; Ex 1.24 TODO
; *******************************************
; Ex 1.25 TODO
; *******************************************
; Ex 1.26-28 TODO (skipping to Section 1.3 for now).
; *******************************************
; SECTION 1.3
; *******************************************
; Ex 1.29 TODO
; *******************************************
; Ex 1.30, sum with iterative process

; Original sum proc using a recursive process:
(define (sum term a next b)
  (if (> a b) 0
      (+ (term a)
         (sum term (next a) next b))))

; sum-iter proc which refactors sum as an iterative process:
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

; Test on "precocious Gauss" example:
; (sum-iter identity 1 ++ 100) ; => 5050
; *******************************************
; Ex 1.31 a, b
; Recursive process first:
; First, "genericize" to allow not just sum or product but other binary ops as well.

(define (apply-op op id term a next b) ; pass in id as well (for identity relative to op). E.g. if op=*, id=1.
  (if (> a b) id
      (op (term a)
         (apply-op op id term (next a) next b))))

; apply-op-iter, iterative process version:
(define (apply-op-iter op id term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (op (term a) result))))
  (iter a id))

; Now that the heavy lifting is done, product and fact are quite simple:
(define (product a b) ; => a * (a+1) * ... * b
  (apply-op * 1 identity a ++ b))
(define (product-iter a b)
  (apply-op-iter * 1 identity a ++ b))
(define fact (λ (n) (product 1 n)))
(define fact-iter (λ (n) (product-iter 1 n)))

; Pi recursive approximation:
(define (piOver8 cutoff) ; divide both sides by 2 first since single 2 in numerator is awkward
  (sq (/ (apply-op * 1 identity 4 (λ (n) (+ n 2)) cutoff)
         (apply-op * 1 identity 3 (λ (n) (+ n 2)) cutoff))))

(define (Pi cutoff) ; cutoff determines how many terms are in the numerator and denominator products.
  (/ (exact->inexact (* 8 (piOver8 cutoff))) cutoff))

; Pi iter approximation:
(define (piOver8-iter cutoff) ; divide both sides by 2 first since single 2 in numerator is awkward
  (sq (/ (apply-op-iter * 1 identity 4 (λ (n) (+ n 2)) cutoff)
         (apply-op-iter * 1 identity 3 (λ (n) (+ n 2)) cutoff))))

(define (Pi-iter cutoff) ; cutoff determines how many terms are in the numerator and denominator products.
  (/ (exact->inexact (* 8 (piOver8-iter cutoff))) cutoff))

;;(display "Pi = ")
;;(Pi-iter 1000) ; => 3.143...

; *******************************************
; Ex 1.32 a (recursive), b (iterative)

; We can use apply-op from 1.31 since it is doing the same thing with different names:
(define (accumulate combiner null-value term a next b)
  (let ([op combiner] [id null-value])
    (apply-op op id term a next b)))

(define (accumulate-iter combiner null-value term a next b)
  (let ([op combiner] [id null-value])
    (apply-op-iter op id term a next b)))

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
