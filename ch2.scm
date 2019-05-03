#lang racket

; To run test code, uncomment the double semi-colons at the end of each section, though not all sections have test cases
; and some have test cases I've left un-commented. It's a bit unorganized, but github is free so you get what you pay for.
; NB: These haven't been tested exhaustively so there could be errors.

(define xs '(42 99 7))    ; for testing
(define (sq x) (* x x))
(define square sq)
(define nil null)

; Some helper functions:
(define (range-fixed a b k) ; => '(a+k, a+2k, ... , b-k, b)
  (let ([step (/ (- b a) k)])
    (range a (+ b step) step)))

(define 1/ (λ (x) (/ 1 x)))
(define mod remainder)
(define negative (λ (x) (- 0 x)))
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
(define (first-two-same? x . xs) ; example of dotted notation
  (if (= x (car xs)) #t #f))
(define (mean xs) (/ (apply + xs) (length xs))) ; arithmetic mean
(define (decades n) (map (lambda (n) (expt 10 n)) (range 0 n))) ; useful for checking O(f(n)) growth, e.g. in prime testing problems
(define (divides? a b) (= (remainder b a) 0))
(define (init xs) ; assume xs non-empty since init is meaningless otherwise
  (if (empty? (cdr xs))
      '()
      (cons (car xs) (init (cdr xs)))))

; *******************************************
; *******************************************
; *******************************************
; Ex 2.4
(define (Cons x y) ; Use upper case so default cons is unaffected
  (λ (m) (m x y)))

(define (Car z) (z (λ (p q) p))) ; Use uppercase so that default car and cdr are unaffected
(define (Cdr z) (z (λ (p q) q)))

#; (define (List x . xs) ; List datatype using this implementation; not mentioned in book, just for fun
     ; Couldn't get this working yet
 ; (if (empty? xs)    (Cons x '()) (Cons x (Cons (Car xs)
  (cond
    ;[(null? x) '()]
    [(empty? xs) (Cons x '())]
;    [else (Cons x (List (Car xs) (Cdr xs)))]))
    [else (Cons x (apply List (Cdr xs)))]))

(define (display-List xs)
  ;(cond
  ;  [(empty? xs) (display '())]
  (unless (empty? xs)
    (begin
       (display (Car xs)) (display ", ")
       (display-List (Cdr xs)))))

(define xs24 (Cons 1 (Cons 2 (Cons 3 '()))))
;; (Car xs24) ; => 1
;; (display-List (Cdr xs24)) ; => 2, 3,
; *******************************************
; Ex 2.5
(define (multiplicity p n) ; => multiplicity of prime factor p of n
  (if (divides? p n) (++ (multiplicity p (/ n p))) 0))

(define (kons a b) ; Change name from cons b/c of namespace clash like in 2.4 (and ditto for car, cdr)
  (* (expt 2 a) (expt 3 b)))

(define (kar p) (multiplicity 2 p))
(define (kdr p) (multiplicity 3 p))

; *******************************************
; Ex 2.6 Mind boggling Church numerals
(define zero
  (λ (f)
    (λ (x) x))) ; zero is identity function by defn, x -> x (regardless of which f is passed)
(define (add-1 n)
  (λ (f)
    (λ (x) (f ((n f) x)))))

(define one (λ (f) (λ (x) (f x))))
;; ((one sq) 3) => (sq 3) => 9
(define two (λ (f) (λ (x) (f (f x)))))
;; ((two sq) 3) => (sq (sq 3)) => (sq 9) => 81
(define (add n1 n2) ; add the Church numerals n1 and n2
  (λ (f)
    (λ (x) ((n1 f) ((n2 f) x)))))

; A few extras for experimenting:
(define three (add one two)) ; (three f) is function f iterated 3 times
(define four (add one three))
(define five (add one four))

; (((add two two) sq) 3) => 43046721 since ((add two two) sq) means apply sq 4 times => (sq(sq(sq(sq 3)))) = 43046721

; (map (zero sq) (range 1 5)) ; => '(1 2 3 4) since (zero sq) is just the identity; would be the same for any other function besides sq

;(define (disp-Ch cn) ; display a Church numeral since the numerals themselves are procedures
  ; using Z for zero and a tick mark added for each value above zero, e.g. ChurchTwo => Z'', so it's a type of unary notation
 
; *******************************************
; Sec 2.1.4

#;(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

#;(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

#;(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

; *******************************************
; TODO: interval problems 2.7-2.16
; *******************************************
; Sec 2.2
(define (make-interval x y) "TODO")

; *******************************************
; Ex 2.17
(define (last-pair xs) ; 
  (if (empty? (cdr xs))
      xs
      (last-pair (cdr xs))))

;; (last-pair (list 23 72 149 34)) ; => '(34)
; *******************************************
; Ex 2.18
(define (vujadeTech-reverse xs) ; reverse is already in #racket namespace so indulging in some shameless self-promotion.
  (if (or (empty? xs) (empty? (cdr xs)))
      xs
      (append (last-pair xs) (vujadeTech-reverse (init xs)))))

; *******************************************
; 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? empty?)

;; (cc 100 us-coins) ; => 292
(define cc-100-us-all-perms ; calculate cc 100 for every permuation of us-coins
  (map (λ (coins-perm) (cc 100 coins-perm)) (permutations us-coins)))
;;(apply max cc-100-us-all-perms) ; => 292
;;(apply min cc-100-us-all-perms) ; => 292
; max and min are both 292, so every cc 100 for every us-coins permuation is 292.
; More generally, cc doesn't depend on the order since the else clause does a brute
; force search by considering both cases: including the first denom or not, regardless of its amount. 
; *******************************************
; Ex 2.20, dotted-tail notation
(define (same-parity x0 . xs)
  (if (empty? xs) (list x0) ; handle trivial case
      (let ([keep? (λ (x) (even? (- x0 x)))] ; keep?(x) iff x0-x is even, which means x0 and x have the same parity
            [x1 (car xs)]) 
        (if (keep? x1)
           ; (cons x0 (cons x1 (same-parity x1 (cdr xs))))
            (cons x0 (apply same-parity (cons x1 (cdr xs))))
            (apply same-parity (cons x0 (cdr xs)))))))

;; (apply same-parity (range 1 8)) ; => '(1 3 5 7)
;; (apply same-parity (range 2 8)) ; => '(2 4 6)
; *******************************************
; Ex 2.21 TODO
; *******************************************
; Ex 2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  ; To deal with the result being reversed, just reverse it before returning:
  (reverse (iter items nil)))

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

