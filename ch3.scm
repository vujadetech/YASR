#lang racket

; For set-car! etc:
;(require r5rs) ; This threw an error - conflicts with srfi/1?
(require rnrs/mutable-pairs-6)

(require srfi/1)
(require "utils-vujadeTech.scm")

; *******************************************
; Sec 3.1 Assignment and local state

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

; We can also create objects that handle deposits as well as withdrawals, and
; thus we can represent simple bank accounts. Here is a procedure that returns
; a ``bank-account object'' with a specified initial balance:

(define (make-account balance)
  (define (withdraw amount) ; This appears to behave like an OO member function. Ditto for deposit.
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ;(define (current-balance) balance)
  (define (dispatch m) ; This appears to function like an OO virtual lookup table. And m means "member"?
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'current-balance) balance)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100)) ; Same as C++ constructor with member variable balance = 100? 
;((acc 'withdraw) 50) ; Same as C++ acc->withdraw(50)?
;((acc 'withdraw) 60)
;((acc 'deposit) 40)
;((acc 'withdraw) 60)

; *******************************************
; Ex 3.1
(define (make-accumulator current) ; Start with current, add delta when calling the function each time.
  (λ (delta)
    ; (+=! current delta)
    (set! current (+ current delta))
    current))

(define A (make-accumulator 5))

; *******************************************
; Ex 3.2
(define (make-monitored f)
  (let ([count (make-accumulator 0)]) 
    (λ (x)
      (if (number? x)
          (begin
            (count 1) ; add 1 to count
            (f x))    ; and return f(x)
          (count 0))  ; return current count
      )))
  
(define s (make-monitored sqrt))

; *******************************************
; Ex 3.3
(define (make-account-with-password balance password)
  (let ([acc (make-account balance)])
    (λ (pass-given m) ; Must provide both a pass and m so acc will know whether to dispatch on
      ; withdraw or deposit.
      (if (eq? pass-given password)
          (acc m)
          (λ (_) "Incorrect password.") ; constant function "Incorrect password."
          ))))

#;(define make-account-with-password-curried
  (λ (password)
    (λ (balance)
      (let ([acc (make-account balance)])
        (λ (pass-given)
          (if (eq? pass-given password)
              acc
              (error "Incorrect password.")
              ))))))

;(define mawph make-account-with-password-h)
#;(define mawpc make-account-with-password-curried)

#;(define (make-account-with-password balance password)
  ((make-account-with-password-curried password) balance))

#;(define (mawp b p)
  ((mawpc p) b))

(define acc-pass (make-account-with-password 100 'secret-password))
;;((acc-pass 'secret-password 'withdraw) 40)    ; => 60
;;((acc-pass 'some-other-password 'deposit) 50) ; => "Incorrect password."
; *******************************************
; Ex 3.4
(define (reset-accumulator a) (a (- (a 0))))
(define call-the-cops (λ (_) "Quick! What's the number for 911!?!?!"))

(define (make-account-with-password-limit balance password)
  (let ([acc (make-account balance)][failed-pass (make-accumulator 0)][fail-pass-limit 7])
    (λ (pass-given m) ; Must provide both a pass and m so acc will know whether to dispatch on
      ; withdraw or deposit.
      (if (eq? pass-given password)
          (begin ; correct pass
            (reset-accumulator failed-pass)
            (acc m))
          (begin ; wrong pass
            (failed-pass 1)
           ; (display (failed-pass 0))
            (if (<= (failed-pass 0) fail-pass-limit)
                (λ (_) "Incorrect password.")
                call-the-cops))))))

(define acc-pass-limit (make-account-with-password-limit 100 'secret-password))
;; (map (λ (pass) ((acc-pass-limit pass 'deposit) 50)) (repeat 'some-other-password 8))
; => '("Incorrect password."
;  "Incorrect password."
;  "Incorrect password."
;  "Incorrect password."
;  "Incorrect password."
;  "Incorrect password."
;  "Incorrect password."
;  "Quick! What's the number for 911!?!?!")
; *******************************************
; Sec 3.1.2 The benefits of introducing assignment,
; the importance of being earnest, and the politics of dancing*.
; I mean just the first one, sorry for that hilarious joke interruption!
; * One of the best 80s songs from the 20th century: https://www.youtube.com/watch?v=sRrSwLHyxGc

; rand-update is from pscholz's github, https://github.com/psholtz/MIT-SICP/tree/master/Section-3.1:
(define m (expt 2 32))
(define a 1664525)
(define b 1013904423)
(define (rand-update x) ; Could also think of this is (next-rand x)
 (let ((m (expt 2 32))
       (a 1664525)
       (b 1013904423))
  (remainder (+ (* a x) b) m)))

; The book didn't say what value to use for random-init.
(define random-init 42)

(define rand
  (let ((x random-init)) ; x is like a C++ member variable here? Not sure but this is interesting.
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi-mc trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
   (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

; [book] Now let us try the same computation using rand-update directly rather than rand, the way we would be forced to proceed if we did not use assignment to model local state:

(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))
(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)   
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else
               (iter (- trials-remaining 1)
                     trials-passed
                     x2))))))
  (iter trials 0 initial-x))

; *******************************************
; Ex 3.5 Monte Carlo integration, which is not to be confused with
; Atlantic City integration, Jersey style. HEY-oh!
; Apparently an Atlantic City algorithm is an actual thing, though my joke
; is still a great idea and I stand by: https://en.wikipedia.org/wiki/Atlantic_City_algorithm.
; The last sentence was inspired by "Focus group" by Tim Robinson: https://www.youtube.com/watch?v=8YDpvMYk5jA

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (random-pair-in-range x1 x2 y1 y2) ; random pair in R2, that is 2D space
  (cons (random-in-range x1 x2) (random-in-range y1 y2)))

(define (hyp^2 x1 x2 y1 y2) ; (x2 - x1)^2 + (y2 - y1)^2
  (+ (sq (- x2 x1)) (sq (- y2 y1))))

(define (in-circle-radius-R? R)
  (λ (x y)
    (< (+ (sq x) (sq y)) (sq R))))

(define icr10? (in-circle-radius-R? 10))
(define square-radius-10 (list -10 10 -10 10))

#;(define (exp10)
  (let ([X1 -10] [X2 10] [Y1 -10] [Y2 10] [P icr10?])
    (let ([x (random-in-range X1 X2)] [y (random-in-range Y1 Y2)])
      (icr10? x y))))

(define (experiment-pi R) ; assume circle is radius R at (0,0), region is square bounding the circle
; experiment is to pick a random (x,y) coordinate in [-R,R] x [-R,R]
  ; and see if it's in the circle of radius R at (0,0).
  (let* ([X1 (negative R)][X2 R][Y1 X1][Y2 X2][P (in-circle-radius-R? R)])
    (let ([x (random-in-range X1 X2)] [y (random-in-range Y1 Y2)])
      (P x y))))

(define exp10 (experiment-pi 10))

(define (estimate-integral P X1 X2 Y1 Y2 N) ; P = predicate, N = # trials
  (let ([total-area (* (- X2 X1) (- Y2 Y1))])
   (monte-carlo N P)))
    
   ; (let ratio (exact->inexact (monte-carlo N P)))

(define (Pi-ei N [R 10])  ; Pi using estimate-integral, N = # trials
  (let* ([X1 (negative R)][X2 R][Y1 X1][Y2 X2][P (in-circle-radius-R? R)])
   ; (estimate-integral (in-circle-radius-R? R) X1 X2 Y1 Y2 N)))
    (estimate-integral
     (λ () (experiment-pi R)) ; experiment must be a function of 0 arguments
      X1 X2 Y1 Y2 N)))
    
; (define PI (exact->inexact (* 4 (monte-carlo 100 exp10))))
;; PI ; => 3.24 Answer is unfluenced by randomness, so YMMV :)

; *******************************************
; Ex 3.6, let's call it rand-with-reset to avoid clobbering rand which is in namespace

(define rand-with-reset
  (let ([x random-init])
    (λ (m)
      (cond
        [(eq? m 'generate)
         (begin
           (set! x (rand-update x))
           x)]
        [(eq? m 'reset)
         (λ (new-seed) (set! x new-seed) new-seed)]
           ))))

;;(map (λ (_) (rand-with-reset 'generate)) (range 1 5))
;;(map (λ (_) (rand-with-reset 'generate)) (range 1 5))
;;((rand-with-reset 'reset) random-init)
;;(map (λ (_) (rand-with-reset 'generate)) (range 1 5))
; Last and first "random" lists should be the same: '(1083814473 711399388 3416838739 1642706014)

; *******************************************
; Ex 3.7

(define bad-pass-response (λ (_) "Incorrect password.")) ; constant function "Incorrect password."

(define (make-joint acc1 pass1 pass2) ; (acc 'pass1) account must already exist and
  ; is pass protected. That is, it needs a pass as well as member function to work.
  (λ (pass2-given m)
    (if (eq? pass2-given pass2)
        (acc1 pass1 m) ; acc1 needs pass1 to work
        bad-pass-response)))

(define peter-acc (make-account-with-password 100 'open-sesame))
(define paul-acc  (make-joint peter-acc 'open-sesame 'rosebud))

;;(paul-acc 'rosebud 'current-balance) ; => 100
;;((paul-acc 'rosebud 'deposit) 25) ; => 125
;;(peter-acc 'open-sesame 'current-balance) ; 125, so peter has 125 b/c paul put in 25

; *******************************************
; Ex 3.8
(define (f x)
  (mod (apply + (flatten (idt x))) 2))

; +_lr is like plus but forces evaluation of e1 first
(define (+_lr e1 e2) ; e1 and e2 are quoted to delay evaluation so they can be ordered.
  (let ([x1 (eval e1)]) ; eval e1 first
    (let ([x2 (eval e2)])
      (+ x1 x2))))

(define (+_rl e1 e2) ; e1 and e2 are quoted to delay evaluation so they can be ordered.
  (let ([x2 (eval e2)]) ; eval e2 first
    (let ([x1 (eval e1)])
      (+ x1 x2))))

(define (make-list-acc xs)
  (λ (ys)
    (set! xs (append ys xs))
    xs))

(define (g x)
  (let ([xs (make-list-acc '())][count (make-accumulator 0)])
    (list (count 1) (xs x))))

(define id (λ (x) x))

(define (make-historied f [starting-history '()]) ; if a number x is sent to f, returns f(x), otherwise all values previously sent to f
  (let ([history (make-list-acc starting-history)])
    (λ (x)
      (if (number? x) ; if x is a num, add x to history and return f(x)
          (begin
            (history (list x))
            (f x))
          (history '()))))) ; else just return history for any input that's not a number, like say the symbol 'get-history 

(define (f2 x)
  (let* ([idh (make-historied id)][curr (idh '())])
      (let ([answer  (if (empty? (idh '())) 0 (car (idh '())))])
        (begin
          (idh x)
          (list answer (idh '()))))))

(define (f3 x)
  (let ([f3-trail (f-trail id)])
   ; (let ([trail (f3-trail x)])
    ;  trail)))
    (f3-trail x)))

#;(define (f-trail f)
  (let ([f-hist (make-historied f)])
    (λ (x)
      (let ([curr (f-hist x)])
        (list curr (f-hist '()))))))

(define (f-trail f)
  (let ([f-hist (make-historied f)])
    (λ (x)
      (f-hist x))))

(define idt (f-trail id))

(define (sq* x)
  (let ([xs '()])
    (set! xs (cons (sq x) xs))
    xs))

;;(+_lr '(f 0) '(f 1)) ; => 0
;;(+_rl '(f 0) '(f 1)) ; => 1

;; (+ (f 0) (f 1)) ; => 0, so + is behaving like it evaluates it's arguments left to right.
; NB: This was failing sometimes b/c it needed the environment reevaluated. See below for more experiments.

(define (f4 x)
  (let* ([curr (idt x)][hist (cdr curr)][prev-hist (if (empty? hist) '() (cdr hist))])
   ; (list hist prev-hist)))
    (let ([prev-x (cond
                    [(empty? (cdr hist)) 0]
                    [else (cadr hist)])])
      (list prev-x hist))))
    
(define (f5 x)
  (let* ([curr (idt x)][hist (cdr curr)])
    (let ([prev-hist (if (empty? hist) '() (cdr hist))])
      (list hist prev-hist))))

(define (f6 x)
  (let* ([prev (idt '())][curr (idt x)])
    (list prev curr)))

(define f7
  (let ([xs-hist (make-historied id)])
    (λ (x)
      (let ([has-prev-x? (not (empty? (xs-hist 'hist)))])
        (let ([prev-x (if has-prev-x? (car (xs-hist 'hist)) 0)]) ; set prev-x to 0 if no history yet
          (xs-hist x) ; add x to history
          (if (not has-prev-x?)
              x ; if no history, it's identity
              (if (zero? x)
                  (++ (negative (f7 1)))
                  (negative (f7 0)))))))))

(define f8
  (let ([idm (make-historied id '(-1))])
    (λ (x)
      (idm x)
      (idm 'hist))))

(define f9
  (let ([idm (make-monitored id)])
    (λ (x)
      (idm x)
      (if (zero? x)
          (++ (idm 'num-calls))
          (negative (idm 'num-calls))))))

(define (f10 x) (/ (f9 x) 2))

; Finally got one that works regardless of function history:
;; (+ (f10 0) (f10 1)) ; => 0
;; (+ (f10 1) (f10 0)) ; => 1 This is like a + that evaluates right to left on the above inputs above.

; *******************************************
; 3.2 The environment model of evaluation

; TL;DR: An ENV is seq of FRAMES what are tables of (possibly empty) BINDINGS (var of num, proc, etc => value)
; and a pointer to its enclosing ENV (unless it's the global ENV). BINDINGS are searched bottum up.

; [From book]
; When we introduced compound procedures in chapter 1, we used the substitution model of evaluation
; (section 1.1.5) to define what is meant by applying a procedure to arguments:

; To apply a compound procedure to arguments, evaluate the body of the procedure with each
; formal parameter replaced by the corresponding argument.
; Once we admit assignment into our programming language, such a definition is no longer adequate.
; In particular, section 3.1.3 argued that, in the presence of assignment, a variable can no longer
; be considered to be merely a name for a value. Rather, a variable must somehow designate a ``place''
; in which values can be stored. In our new model of evaluation, these places will be maintained in
; structures called environments.

; An environment is a sequence of frames. Each frame is a table (possibly empty) of bindings,
; which associate variable names with their corresponding values. (A single frame may contain at most
; one binding for any variable.) Each frame also has a pointer to its enclosing environment,
; unless, for the purposes of discussion, the frame is considered to be global.
; The value of a variable with respect to an environment is the value given by the binding of the
; variable in the first frame in the environment that contains a binding for that variable.
; If no frame in the sequence specifies a binding for the variable, then the variable is said
; to be unbound in the environment. 

; *******************************************
; Ex 3.9, NA show environment structures created
; *******************************************
; Ex 3.10 NA show ENVs

; Recall from section 1.3.2 that let is simply syntactic sugar for a procedure call:

; (let ([<var> <exp>])
;   <body>) is interpreted as an alternate syntax for

; ((λ (<var>) <body>) <exp>)

; *******************************************
; Ex 3.11 NA show ENVs
; *******************************************
; Ex 3.12 See ch3-lang-sicp.scm.



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
