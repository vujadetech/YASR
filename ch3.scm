#lang racket
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
  (define (dispatch m) ; This appears to function like an OO virtual lookup table. And m means "member"?
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
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
#;(define (make-monitored f) ; f takes one input
  (let ([count 0])
    (λ (x)
      (define (dispatch m)
        (cond
          [(eq? m 'how-many-calls?) count]
          [else (begin
                  (++ count)
                  (f x))]))
      dispatch)))

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
