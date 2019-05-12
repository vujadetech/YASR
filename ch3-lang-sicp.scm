#lang sicp

(define (-- k) (- k 1))

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

;(define x (list 'a 'b))
;(define y (list 'c 'd))
;(define z (append x y))
;z
; (a b c d) normal append
;(cdr x)
; <response> (b) like normal
;(define w (append! x y))
;w
; (a b c d)
;(cdr x)
; <response> '(b c d) b/c x was changed so it pointed to y after the b.

; *******************************************
; Ex 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (make-cycle-car x) ; Another way to make cycles where the car has the loop back to itself rather than the cdr.
  (set-car! (last-pair x) x))

(define z13 (make-cycle (list 'a 'b 'c))) ; z13 is a loop
;;(cadddr z13) ; => 'a since it loops back to beginning after 3 cdrs,
; and will keep looping forever if (cdr z13) is called.

; *******************************************
; Ex 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x) ; nothing to reverse, so...
        y         ; ... return what's been accumulated.
        (let ((temp (cdr x))) ; temp is what will be reversed next
          (set-cdr! x y) ; y is accumulator so (cdr x) will become what's been reversed so far, namely y.
          (loop temp x)))) ; loop next part and accumulate it to x
  (loop x '())) ; start by accumulating on empty list

; It's reversing the list x with y serving as an internal accumulator.

; Un ejemplo que puede clarificar mystery : señor-misterioso
; [An example that can clarify mystery]

(define (señor-misterioso xs)
  (define (rev-acc xs acc)
    (if (null? xs) ; nothing to reverse, so...
        acc         ; ... return what's been accumulated.
        (let ((xs-to-be-reversed (cdr xs))) ; (cdr xs) is what will be reversed next
          (set-cdr! xs acc) ; acc is accumulator so (cdr xs) will become what's been reversed so far, namely acc.
          ; So now xs is the new acc:
          (rev-acc xs-to-be-reversed xs)))) ; rev-acc next part and accumulate it to the new xs
  (rev-acc xs '())) ; start by accumulating on empty list

(define reverse-vujadeTech señor-misterioso) ; Mutate/destructive version of reverse which should be more efficient.
; TODO: test efficiency against non-destructive version and built in.
; TODO: Destructive versions of others, e.g. filter, map, flatten, etc
; *******************************************
; Sharing and identity
(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))
(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)
;(set-to-wow! z1)
;(set-to-wow! z2)
; *******************************************
; Ex 3.16

(define (count-pairs-ben-bitdiddle x)
  (if (not (pair? x))
      0
      (+ (count-pairs-ben-bitdiddle (car x))
         (count-pairs-ben-bitdiddle (cdr x))
         1)))

(define z3 (cons 1 (cons 2 (cons 3 '()))))
(count-pairs-ben-bitdiddle z3) ; => 3, which is the correct number of pairs in this case.
(define z4 (cons x (cdr x))) ; x is 2 pairs, so z4 makes a 3rd pair...
(count-pairs-ben-bitdiddle z4) ; => 4 rather than 3, which means count-pairs is wrong,
; though with a name like "Ben Bitdiddle" that's not a surprise. HEY-oh!

(define x7 (cons 1 2))
(define y7 (cons x7 x7))
(define z7 (cons y7 y7))
(count-pairs-ben-bitdiddle z7) ; => 7, rather than 3. This is just modifying z1 above to make it seem like it
; has more pairs than z1 even though it doesn't.

; Construct the "never return at all" with big z-cycle in parts: Z1 and Z2
(define Z1 (cons 'a '()))
(define Z2 (cons 'b (cons 'c '())))
Z1
Z2
(set-cdr! Z1 Z2)
(set-cdr! (cdr Z2) Z1)
(define z-cycle Z1)
;; (count-pairs-ben-bitdiddle z-cycle) ; => starts infinite loop
; *******************************************
; Ex 3.17

; See community.schemewiki.org for this solution.
; Changed name to do some shameless self-promotion :D
; If I make any money off it, I'll be happy to share it with the fine folks at schemewiki.org.
; [Schemewiki.org mini-commercial]
; Do you like Scheme? And do you like wikis? Then you're gonna LOOOOOOOOVE schemewiki.org!!!
(define (count-pairs-vujadeTech q) ; Define helper function after acc defined so it will keep updating it.
  ; Call it q rather than p since it might not be a pair.
  ; The key is defining the accumulator with a let and then defining a helper function.
  (let ([acc '()])
    (define (count-help q)
      (cond
        [(not (pair? q)) 0]
        [(memq q acc) 0] ; already accounted for
        [else
           (set! acc (cons q acc))
         ; (display acc) (newline) ; In case you want to see the acc as it develops.
           (+ (count-help (car q))
              (count-help (cdr q))
              1)]))
    (count-help q)))

;; (count-pairs-vujadeTech z7) ; => 3
; *******************************************
; 3.18
(define count-pairs count-pairs-vujadeTech)

#;(define (has-cycle? x)
  (let ([acc '()])
    (define (has-cycle-h x)
      (cond
        [(not (pair? x)) #f]
       ; [(memq (cdr x) acc)    #t] ;
       ; [(mem
        ;[(memq x acc)    #t]
        [
         ;(or (memq (car x) acc)
             (memq (cdr x) acc)
          ;   )
         #t]
        [else
         (set! acc (cons x acc))
         (or (has-cycle-h (car x)) (has-cycle-h (cdr x)))]))
    (has-cycle-h x)))

(define (has-car-eq-y? x y)
  (cond
    [(not (pair? x)) #f]
    [(eq? (car x) y) #t]
    [else (or (has-car-eq-y? (car x) y) (has-car-eq-y? (cdr x) y))]))

(define (can-reach-itself-k? v k) ; Can node v reach itself within k steps, k > 0?
  (cond
    [(zero? k) #f]
    [(= 1 k) (or (eq? v (car v)) (eq? v (cdr v)))]
    [else (reachable-k? (car v) v (-- k)) (reachable-k? (cdr v) v (-- k))]))

(define (reachable-k? u v k); is node (pair) v reachable from u in <= k steps where k >= 0?
  (cond
    [(eq? u v)        #t] ; if u = v then k is irrelevant since they're the same node which means that v is vacuously reachable from u.
    [(zero? k) (eq? u v)] ; if k = 0 they must be the same node to be reachable.
    [(not (pair? u))  #f] 
    [(= 1 k) (or (eq? (car u) v) (eq? (cdr u) v))]
    [else (or (reachable-k? (car u) v (-- k)) (reachable-k? (cdr u) v (-- k)))]))

(define (can-reach-itself? v)
  (let ([k (count-pairs v)])
    (can-reach-itself-k? v k)))

(define has-cycle? can-reach-itself?)

(define w3 '(a b c))
(make-cycle w3)

; *******************************************
; *******************************************
; *******************************************
; *******************************************
; *******************************************
; *******************************************
; *******************************************