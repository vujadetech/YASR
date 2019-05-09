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
#;(define (make-account-with-password balance password)
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

(define (make-account-with-password balance password)
  (let ([acc (make-account balance)])
    (λ (pass-given m) ; Must provide both a pass and m so acc will know whether to dispatch on
      ; withdraw or deposit.
      (if (eq? pass-given password)
          (acc m)
          (λ (_) "Incorrect password.") ; constant function "Incorrect password."
          ))))

(define make-account-with-password-curried
  (λ (password)
    (λ (balance)
      (let ([acc (make-account balance)])
        (λ (pass-given)
          (if (eq? pass-given password)
              acc
              (error "Incorrect password.")
              ))))))

;(define mawph make-account-with-password-h)
(define mawpc make-account-with-password-curried)

#;(define (make-account-with-password balance password)
  ((make-account-with-password-curried password) balance))

#;(define (mawp b p)
  ((mawpc p) b))

(define acc-pass (make-account-with-password 100 'secret-password))

#;(define acc-pass-c
  ((make-account-with-password-curried 'secret-password) 100)) ; curried version
((acc-pass 'secret-password 'withdraw) 40)
((acc-pass 'some-other-password 'deposit) 50) 

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
