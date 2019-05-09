#lang racket
(require srfi/1)
(require "utils-vujadeTech.scm")
; (require sicp-pict)

; To run test code, uncomment the double semi-colons at the end of each section, though not all sections have test cases
; and some have test cases I've left un-commented. It's a bit unorganized, but github is free so you get what you pay for.
; NB: These haven't been tested exhaustively so there could be errors.

; Some helper functions:
; These are now in the module utils-vujadeTech.scm.

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
(define three (add one two))  ; (three f) is function f iterated 3 times
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
(define (leaf-node? t) (not (pair? t)))

(define (fringe t)
  (cond
    [(empty-tree? t) empty-tree]
    [(leaf-node? (car t)) (cons (car t) (fringe (cdr t)))]
    [else (append (fringe (car t)) (fringe (cdr t)))]))

; *******************************************
; Ex 2.29, this is obviously too complicated, could be the agent orange of code smells.
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
      
;;(mobile-is-balanced? m1) ; => #t
;;(mobile-is-balanced? m2) ; => #f
;;(mobile-is-balanced? M1) ; => #t
;;(mobile-is-balanced? M2) ; => #t

; d. TODO
; Skipping part d for now since I'm a bit "mobiled out" at this point.

; *******************************************
; Ex 2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((leaf-node? tree) (sq tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

;;(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
            
(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (not (leaf-node? sub-tree))
             (square-tree-map sub-tree)
             (sq sub-tree)))
       tree))

;;(square-tree-map (list 1 (list 2 (list 3 4) 5) (list 6 7)))

; *******************************************
; Ex 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (not (leaf-node? sub-tree))
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (sq-tree tree) (tree-map sq tree))
;;(sq-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

; *******************************************
; Ex 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (λ (x) (cons (car s) x)) rest)))))

; *******************************************
; Sec 2.2.3
(define (filter_ predicate sequence) ; filter already in namespace
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter_ predicate (cdr sequence))))
        (else (filter_ predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

; *******************************************
; Ex 2.33, put on your big boy pants, we're gettin' ready to do some functional programming.
; Sure we've been gettin' a little "funcy" so far, but
; defining our own versions of map, append, length - people if that ain't functional programming,
; I don't know what is!
; ... deep breath ... calm blue oceans

(define (map-vujadeTech p sequence) ; map already in namespace, ditto for append and length below
  (accumulate (lambda (x y) (p x y)) nil sequence))
(define (append-vujadeTech seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length-vujadeTech sequence)
  (accumulate (λ (x y) (+ 1 y)) 0 sequence))

; *******************************************
; Ex 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))

(define (dot xs ys) ; for testing horner-eval
  (let ([zs (map (λ (z) (apply * z)) (zip xs ys))])
    (accumulate
     (λ (z w) (+ z w))
     0
     zs)))

(define coeffs (list 1 3 0 5 0 1))
;;(horner-eval 2 coeffs) ; 1 + 3x ... for x=2 ; => 79
(define 2s (powers 2 5))
;;(dot 2s coeffs) ; => 79

; *******************************************
; Ex 2.35
(define (count-leaves-acc-enum t) ; using accumlate with enumerate-tree, different than book's suggestion
  (accumulate
   (λ (_ rest-count) (++ rest-count)) ; op
   0 ; initial
   (enumerate-tree t))) ; sequence

; *******************************************
; Ex 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (λ (xs) (car xs)) seqs)) ; accumulate on cars of all xs in seqs
            (accumulate-n op init (map (λ (xs) (cdr xs)) seqs))))) ; then recur on cdrs of all xs in seqs

;; (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) ; => '(22 26 30)

; *******************************************
; Ex 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (λ (row) (dot-product v row)) m))
(define (transpose mat)
  (accumulate-n cons '() mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (λ (m_row) (matrix-*-vector cols m_row))  m)))

(define mat1 '((1 2 3 4) (4 5 6 6) (6 7 8 9))) ; 3x4 matrix
(define v1 '(1 -2 3 -4))
(define mat2 '((1 2) (3 4) (6 7) (8 9))) ; 4x2 matrix
(define mat2T (transpose mat2)) ; '((1 3 6 8) (2 4 7 9))
;;(matrix-*-matrix mat1 mat2) ; => '((57 67) (103 124) (147 177)), though I'm too lazy to check if that's right, lol

; *******************************************
; Ex 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)
;;(fold-right / 1 (list 1 2 3)) ; => 1 / (acc (2 3)) => (/ 1 (/ 2 (/ 3 1))) => 3/2
;;(fold-left  / 1 (list 1 2 3)) ; => (iter 1 (1 2 3)) => (iter (/ 1 1) (2 3)) =>
; (iter 1 (2 3)) => (iter (/ 1 2) (3)) => (iter 1/2 (3)) => (iter (/ 1/2 3) '()) => 1/6
;;(fold-right list nil (list 1 2 3)) 
;;(fold-left list nil (list 1 2 3))
; general cases in infix notation using id (identity) for initial value
; since that's the most common case: 0 for adding nums, '() for  for concatenating lists, etc.
;  (fold-left op id '(x1 ... x_N)) => ((((id op x1) op x2) ... ) op x_N
;  that is, put id in front of the xs and left associate op.
;  (fold-right op id '(x1 ... x_N) =>   (x1 op (x2 op ... op (x_N-1  op (x_N op id))))
;  put id at end of xs and right associate op.
; op should be commutative to ensure that fold-left and fold-right produce the same values.
; Neither / nor list are commutative, hence the different results for left vs right folds.

; *******************************************
; Ex 2.39
(define (rev-fold-right sequence) ; reverse using fold-right
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(define (rev-fold-left sequence) ; reverse using fold-left
  (fold-left (lambda (x y) (cons y x)) nil sequence))

; *******************************************
; Nested mappings

(define (nest-pair n)
  (accumulate append
            nil
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

; *******************************************
; TODO 2.40-43
; *******************************************
; Sec 2.2.4 A picture language
; TODO
; *******************************************
; 2.3 Symbolic Data
; *******************************************
; Ex 2.53, N/A
; *******************************************
; Ex 2.54
; The book didn't mention whether lists might have numbers, so ignoring that possibility.

(define (one-list-null-other-not? xs ys) ; assumes xs and ys are both lists.
  (or (and (null? xs) (not (null? ys))) (and (null? ys) (not (null? xs)))))

(define (vujadeTech-equal? a b) ; equal? already in namespace
  (cond
    [(and (symbol? a) (symbol? b)) (eq? a b)]
    [(and (list? a) (list? b) (null? a) (null? b)) #t] ; all empty lists ar equal
    [(and (list? a) (list? b) (one-list-null-other-not? a b)) #f]
    [(and (list? a) (list? b)) ; if this point is reached a and b are both non-empty lists.
     (and (vujadeTech-equal? (car a) (car b))
          (vujadeTech-equal? (cdr a) (cdr b)))]
    [else #f])) ; if this stage is reached then one is a symbol and other is list, hence unequal.
     
;; (vujadeTech-equal? '(this is a list) '(this is a list))   ; => #t
;; (vujadeTech-equal? '(this is a list) '(this (is a) list)) ; => #f
; *******************************************
; Ex 2.55 I'm not sure why it prints back quote, but apparently ''a is a list,
; so it has a car and cdr, unlike 'a.
; *******************************************
; Sec 2.3.2 Symbolic differentiation

(define (variable? x) (symbol? x))

; Two variables are the same if the symbols representing them are eq?:
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; Sums and products are constructed as lists:
(define (make-sum a1 a2) (list '+ a1 a2))
;;; (define (make-product m1 m2) (list '* m1 m2))
(define (make-product m1 . ms)
  (if (singleton-list? ms)
      (list '* m1 (car ms))
      (list '* m1 (apply make-product ms))))

; A sum is a list whose first element is the symbol +:
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

; The addend is the second item of the sum list:
(define (addend s) (cadr s))

; The augend is the third item of the sum list:
(define (augend s) (caddr s))

; A product is a list whose first element is the symbol *:
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

; The multiplier is the second item of the product list:
(define (multiplier p) (cadr p))

; The multiplicand is the third item of the product list:
;;; (define (multiplicand p) (caddr p))
(define (multiplicand p)
  (if (= (length p) 3) ; binary product, e.g., '(* x y), multiplicand = 'y, which is caddr
      (caddr p)
      ; else it has at least 3 values to be multiplied, so recur on the second and beyond:
      (apply make-product (cddr p))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        
        ; added for Ex 2.56, exponentiation:
        ((exponentiation? exp)
         (let ([u (base exp)] [n (exponent exp)]) ; D(u^n)
           (cond
             [(= 0 n) 0]
             [(= 1 n) (deriv u var)]
             [else
              (make-product (make-product n (make-exponentiation u (- n 1))) (deriv u var))]
             )))

        (else
         (error "unknown expression type -- DERIV" exp))))
; *******************************************
; Ex 2.56, see deriv for exponentiation? section above
(define (exponentiation? x)
  (and (pair? x) (or (eq? (car x) '**) (eq? (car x) '^)))) ; double star or caret style exponents

(define (base exponentiation) (cadr exponentiation))
(define (exponent exponentiation) (caddr exponentiation))

(define (make-exponentiation b e) (list '** b e)) ; b^e or b**e

;; (deriv '(** x 3) 'x) ; => '(* (* 3 (** x 2)) 1)
; *******************************************
; Ex 2.57, changed in Sec 2.3.2 code
; Only product done so far.
; *******************************************
; Sec 2.3.3 Sets
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; *******************************************
; Ex 2.59
(define (union-set set1 set2)
  (cond ((or (null? set1) (null? set2)) (append set1 set2))
        ((element-of-set? (car set1) set2)        
        ; (cons (car set1) (intersection-set (cdr set1) set2)))
         (union-set (cdr set1) set2)) ; (car set1) already in set2, keep going on (cdr set1)
        (else (cons (car set1) (union-set (cdr set1) set2)))))

; *******************************************
; Ex 2.60, TODO
; *******************************************
; Ex 2.61, adjoin-ordered-set
(define (element-of-ordered-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-ordered-set x set)
  (cond
    [(element-of-ordered-set? x set) set]
    [(< x (car set)) (cons x set)]
    [(> x (car set)) (cons (car set) (adjoin-ordered-set x (cdr set)))]))

;; (adjoin-ordered-set 3 '(2 4)) ; => '(2 3 4)

; *******************************************
; Ex 2.62, union-ordered-set

(define (union-ordered-set set1 set2)
  (if (or (null? set1) (null? set2))
      (append set1 set2)
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond
          ((= x1 x2) (union-ordered-set (cdr set1) set2)) ; x1 already there
          ((< x1 x2) (cons x1 (union-ordered-set (cdr set1) set2)))
          ((< x2 x1) (cons x2 (union-ordered-set set1 (cdr set2))))))))

;; (union-ordered-set '(1 3 9) '(2 8)) ; => '(1 2 3 8 9)
; *******************************************
; Sets as binary trees TODO

; *******************************************
; Sec 2.3.4 Huffman Encoding Trees
; See file ch2-huffman.scm due to namespace conflicts.

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

