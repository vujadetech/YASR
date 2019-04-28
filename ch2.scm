#lang racket

; To run test code, uncomment the double semi-colons at the end of each section, though not all sections have test cases
; and some have test cases I've left un-commented. It's a bit unorganized, but github is free so you get what you pay for.
; NB: These haven't been tested exhaustively so there could be errors.

(define xs '(42 99 7))    ; for testing
(define (sq x) (* x x))

; Some helper functions:
(define (range-fixed a b k) ; => '(a+k, a+2k, ... , b-k, b)
  (let ([step (/ (- b a) k)])
    (range a (+ b step) step)))
  
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


; *******************************************
; *******************************************
; *******************************************
; *******************************************
; Ex 2.5
(define (multiplicity p n) ; => multiplicity of prime factor p of n
  (if (divides? p n) (++ (multiplicity p (/ n p))) 0))

(define (conz a b) ; cons already in namespace of course, so call it conz. Also b/c I'm a bad spellir.
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
; e.g., (add-1 sq) => 

;(λ (f) (λ (x) (f ((zero f) x))))
;(λ (f) (λ (x) (f (identity x))))
;(λ (f) (λ (x) (f x)))
(define one (λ (f) (λ (x) (f x))))
;; ((one sq) 3) => (sq 3) => 9
(define two (λ (f) (λ (x) (f (f x)))))
;; ((two sq) 3) => (sq (sq 3)) => (sq 9) => 81
(define (add n1 n2) ; add the Church numerals n1 and n2
  (λ (f)
    (λ (x) ((n1 f) ((n2 f) x)))))

(define three (add one two)) ; (three f) is function f iterated 3 times
(define five (add two three))

; (((add two two) sq) 3) => 43046721 since ((add two two) sq) means apply sq 4 times => (sq(sq(sq(sq 3)))) = 43046721

;(define ChurchOne (add-1 zero))
;(define ChurchTwo (add-1 (add-1 zero)))

; (map (zero sq) (range 1 5)) ; => '(1 2 3 4) since (zero sq) is just the identity; would be the same for any other function besides sq

;(define (disp-Ch cn) ; display a Church numeral since the numerals themselves are procedures
  ; using Z for zero and a tick mark added for each value above zero, e.g. ChurchTwo => Z'', so it's a type of unary notation
 
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

