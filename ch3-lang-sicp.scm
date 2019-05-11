#lang sicp

; *******************************************
; Ex 3.12
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define xs '(1 2))
(define ys '(3))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
; (a b c d) normal append
(cdr x)
; <response> (b) like normal
(define w (append! x y))
w
; (a b c d)
(cdr x)
; <response> '(b c d) b/c x was changed so it pointed to y after the b.

; *******************************************
; Ex 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z13 (make-cycle (list 'a 'b 'c))) ; z13 is a loop
;;(cadddr z13) ; => 'a since it loops back to beginning after 3 cdrs,
; and will keep looping forever if (cdr z13) is called.


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