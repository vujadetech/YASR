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
; Ex 2.23, vujadeTech-for-each since for-each is in namespace
(define (vujadeTech-for-each proc xs)
  (unless (empty? xs)
    (begin
      (proc (car xs))
      (vujadeTech-for-each proc (cdr xs)))))

;; (vujadeTech-for-each (λ (x) (newline) (display x)) (list 57 321 88))
 
; *******************************************
; Sec 2.2.2
(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define tree1 (cons (list 1 2) (list 3 4)))

; *******************************************
; Ex 2.24-2.26: TODO
; *******************************************
; Ex 2.27
(define (deep-reverse xss) ; xss can be a list of xs, that is a list of lists, as well as an ordinary list.
  (cond
    [(null? xss) null]
    [(not (pair? (car xss))) (append (deep-reverse (cdr xss)) (list (car xss)))]
    [else (append (deep-reverse (cdr xss)) (list (deep-reverse (car xss))))]))
    
;; (deep-reverse '( (1 2) (3 4) )) ; => ((4 3) (2 1))

; *******************************************
; Ex 2.28
; First some tree-oriented helpers:
(define empty-tree? null?)
(define empty-tree '())
(define (leaf? t) (not (pair? t)))

(define (fringe t)
  (cond
    [(empty-tree? t) empty-tree]
    [(leaf? (car t)) (cons (car t) (fringe (cdr t)))]
    [else (append (fringe (car t)) (fringe (cdr t)))]))

; *******************************************
; Ex 2.29, this is obviously too complicated, could be the agent orange of code smells
; I mean I heard mobile programming is hard, but this is ridiculous. HEY-oh!

(define (make-mobile left right) ; left and right are branches
  (list left right))

(define (make-branch length structure) ; length is any number, structure is either a num (for weight) or another mobile
  (list length structure))

; a.
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

; b.
(define (total-weight-of-structure structure) ; structure could be number or mobile
  (if (number? structure)
      structure
      (+ (total-weight-of-structure (left-branch structure))
         (total-weight-of-structure (right-branch structure)))))

(define (total-weight-of-branch b)
  (if (number? (branch-structure b))
      (branch-structure b)
      (total-weight (branch-structure b))))

(define (total-weight m) ; m is the mobile
  (let ([left (left-branch m)] [right (right-branch m)])
    (let ([left-structure (branch-structure left)]
          [right-structure (branch-structure right)])
      (+ (total-weight-of-structure left-structure)
         (total-weight-of-structure right-structure)))))

#; (define (total-weight-generic x) ; returns weight whether x is a branch or a mobile
  (if (number)))

(define b1 (make-branch 2 5))
(define b2 (make-branch 1 10))
(define m1 (make-mobile b1 b2)) ; mobile is balanced, weight = 15
(define b3 (make-branch 4 3))   ; torque = 12
(define b4 (make-branch 4 m1))  ; torque = 60
(define m2 (make-mobile b3 b4)) ; mobile is UNbalanced b/c torque of each branch differs

(define B1 (make-branch 2 10)) ; length 2, structure=weight 10
(define B2 (make-branch 5 4))
(define M1 (make-mobile B1 B2)) ; M1 weight = 14
(define B3 (make-branch 4 M1))  ; B3 torque = 4*14 = 56
(define B4 (make-branch 7 8))   ; B4 torque = 56
(define M2 (make-mobile B4 B3)) ; M2 is balanced

; c.
(define (number-branch? b) ; return #t iff branch b is just a number weight
  (number? (branch-structure b)))

(define (mobile-branch? b) (not (number-branch? b))) ; b is a branch which is itself another mobile

(define (mobile-depth m)
  (++ (max (branch-depth (left-branch m)) (branch-depth (right-branch m)))))

(define (branch-depth b)
  (cond
    [(number-branch? b) 0]
    [else ; b is a mobile, so its branch-structure is a mobile which we can call m
     (let ([m (branch-structure b)])
       (mobile-depth m))]))
       
#;(define (depth m) ;
  (let ([left (left-branch m)] [right (right-branch m)])
    (cond
      [(and (number? (branch-structure left)) (number? (branch-structure right))) 1]
      [(number? (branch-structure left)) (++ (depth (branch-structure right)))]
      [else (+ (depth (branch-structure left)) (depth (branch-structure right)))])))

(define (simple-mobile? m) (= 1 (mobile-depth m)))
(define (simple-branch? b) (= 0 (branch-depth b))) ; This is redundant with number-branch? TODO, refactor

(define (torque b)
  (if (number-branch? b)
      (* (branch-length b) (branch-structure b))
      (* (branch-length b) (total-weight (branch-structure b)))))

(define (mobile-is-balanced? m)
  (let ([left (left-branch m)] [right (right-branch m)])
    (cond
      [(simple-mobile? m) (= (torque left) (torque right))]
      [else ; not a simple mobile, so at least one branch has a submobile
       (let ([1st-level-balanced? (= (torque left) (torque right))])
         (and 1st-level-balanced? (branch-is-balanced? left) (branch-is-balanced? right)))])))

(define (branch-is-balanced? b) ; number-branch is balanced, and o.w. b's submobile must be recursively balanced
  (if (number-branch? b)
      #t
      (mobile-is-balanced? (branch-structure b)))) ; b is a branch so it's branch-structure is a mobile
      
(mobile-is-balanced? m1) ; => #t
(mobile-is-balanced? m2) ; => #f
(mobile-is-balanced? M1) ; => #t
(mobile-is-balanced? M2) ; => #t

; d. TODO
; Skipping part d for now since I'm a bit "mobiled out" at this point.

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

