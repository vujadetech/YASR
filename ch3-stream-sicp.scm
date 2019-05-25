#lang sicp
(#%require "modules/utils-vujadeTech.scm")
;(require racket/stream)


; *******************************************
; Section 3.5.1 Streams are delayed lists

(define ! not)

(define (enumerate-interval a b)
  (if (>= a b) '() (cons a (enumerate-interval (+ a 1) b))))

(define false #f) (define true #t)

(define (sum-primes a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))

#;(car (cdr (filter prime?
                  (enumerate-interval 10000 1000000))))


(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (delay exp) (memo-proc (lambda () exp)))
; Had to rewrite this since you can't just call memo-proc on delay,
; as they pointed out in a memoization exercise previously.
; Sometimes boilerplate is a good thing? Anyhoo, sorry about
; the DELAY in getting this ... HEY-oh!
#;(define (delay exp)
  (let ([already-run? false][result false])
    (lambda ()
      (if (! already-run?)
          (begin
            (set! result ((lambda () exp)))
            (set! already-run? true)
            result)
          result))))
; After fixing cons-stream with the syntax rules, it appears you
; can just define delay with memo-proc, but leaving the above code in (commented out)
; so I don't have to get rid of that hilarious "delay" pun.

(define (force delayed-object) (delayed-object))

;(define (cons-stream a b) (cons a (delay b))) ; cons-stream promise
; Apparently racket does eager eval, so need to add a syntax rule:
(define-syntax cons-stream ; see stack-exchange "how-is-the-sicp-cons-stream-implemented"
  (syntax-rules ()
    ((cons-stream a b) (cons a (memo-proc (lambda () b))))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream))) ; steam-cdr = dream realized

(define the-empty-stream '())
(define (stream-null? s) (null? s))
(define stream-empty? stream-null?)

; p435 SICP
(define (stream-enumerate-interval low high)
  (if (>= low high) the-empty-stream
      (cons-stream low (stream-enumerate-interval (++ low) high))))

#;(define (stream-filter pred stream)
  (if (stream-null? stream) the-empty-stream
      (let ([car-stream (stream-car stream)][cdr-filtered (stream-filter pred (stream-cdr stream))])
        (cond
          [(pred car-stream) (cons-stream car-stream cdr-filtered)]
          [else cdr-filtered]))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

; *******************************************
; stream imp in action, p435

(define (stream-prime low high)
  (stream-car
   (stream-cdr
    (stream-filter prime?
                   (stream-enumerate-interval low high)))))

; stream-prime works at the same speed no matter what
; the second parameter is, even if it's one bajillion:
;(stream-prime 10000 100000000000000) ; => 10009

; *******************************************
; Exercise 3.50
; single stream map
#;(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

; generalizaton to an arbitrary number of argstreams for n-ary procs (binary, ternary, yada yada)
(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (prime-stream low high) (stream-filter prime? (stream-enumerate-interval low high))) ; primes from 2 to a million
(define primes-to-million (prime-stream 2 1000000))
(define nats-to-million (stream-enumerate-interval 1 1000000))
(define add-prime-to-nat (stream-map + primes-to-million nats-to-million))
(define (stream-take stream k) ; make a normal list up to k elements of a stream
  (if (zero? k) '()
      (cons (stream-car stream) (stream-take (stream-cdr stream) (-- k)))))

(define add-nats (stream-map + nats-to-million nats-to-million))
;add-nats ; => (2 . #<procedure:...-stream-sicp.scm:27:4>)
;(stream-take add-nats 10) ; => (2 4 6 8 10 12 14 16 18 20)

; *******************************************
; Ex 3.51
(define (show x)
  (display x)
  x)

; What does the interpreter print in response to evaluating each expression in the following sequence?59

;(define x (stream-map show (stream-enumerate-interval 0 100)))
;(stream-ref x 5)
;(stream-ref x 7)

; *******************************************
; Ex 3.52

; I'm gonna guess that it would give different results, since it
; would "forget" that it had already been run, and then keep doing the side-effects.

; *******************************************
; Sec 3.5.2 Infinite Streams

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
;(define fibs (fibgen 0 1))

(define (stream-scale stream k) ; all stream values multiplied by k
  (stream-map (lambda (x) (* k x)) stream))
(define scale-stream stream-scale)

(define evens2mil (stream-scale nats-to-million 2))
(define (stream-remove-multiples k stream) ; stream with multiples of k filtered out
  (stream-filter (lambda (x) (! (divides? k x))) stream))

(define odds (stream-remove-multiples 2 integers))

(define (stream-remove-multiples-of-car stream)
  (let ([kar (stream-car stream)])
    (stream-remove-multiples kar stream)))

(define (stream-foldl proc e stream)
  "TODO")

#;(define (sieve stream)
 ; (iffy (< (stream-car stream) 2) (error "Sieving on a stream whose car < 2."))
  (cond
    [(stream-empty? stream) the-empty-stream]
    [else
     (let ([stream1 (stream-remove-multiples (stream-car stream) stream)])
       stream1)]))

; I decided to refactor SICP's sieve for some reason.
; All due respect to Mr. Eratosthones, of course.
#;(define (sieve stream)
  (cond
    [(stream-empty? stream) the-empty-stream]
    [else
      (let* ([stream1 (stream-remove-multiples-of-car stream)] ; stream1 is odds >= 3 on first run
             [kar (stream-car stream1)])
       (cons-stream kar (sieve (cons kar (stream-cdr stream1)))))]))

#;(define (sieve stream) ; assume infinite stream since filtering composites will leave an infinite list
  (let* ([kar (stream-car stream)][srmoc (stream-remove-multiples-of-car stream)])
    (cons-stream kar (sieve srmoc))))

(define (sieve stream) 
  (cons-stream (stream-car stream) (sieve (stream-remove-multiples-of-car stream))))

(define (stream-fold-right f acc stream)
  (if (stream-null? stream) acc
      (f (stream-car stream) (stream-fold-right f acc (stream-cdr stream)))))

(define 2andUp (integers-starting-from 2))
(define primes (sieve 2andUp))

(define ones (cons-stream 1 ones))

; *******************************************
; brain teaser: what if you do (stream-remove-multiples 1 integers)?
; *******************************************
; Defining streams implicitly

(define (add-streams s1 s2) (stream-map + s1 s2))

#;(define fibs ; fibs implicit
  (cons-stream 0 (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

(define fibs ; fibs implicit order changed, works the same
  (cons-stream 0 (cons-stream 1 (add-streams fibs (stream-cdr fibs) ))))

; *******************************************
; Ex 3.53
; Just duplicate s which is two streams that start with 1, so then is 2.
; Then both have 2 so sum is 4. Then both have 4 so sum is 8,
; yada yada, it's powers of 2 baybee!

(define s (cons-stream 1 (add-streams s s)))
; (stream-take s 10) ; => (1 2 4 8 16 32 64 128 256 512)

; *******************************************
; Ex 3.54

(define (mul-streams s1 s2) (stream-map * s1 s2))

; Sometimes the best way is the most obvious way.
(define factorials
  (cons-stream 1 (mul-streams integers factorials)))
;(stream-take factorials 10) ; => (1 1 2 6 24 120 720 5040 40320 362880)

; *******************************************
; Ex 3.55

(define (partial-sums stream) ; nth element of stream is nth partial sum
  (cons-stream (stream-car stream)
               (add-streams (stream-cdr stream) (partial-sums stream))))

;; (stream-take (partial-sums integers) 10) ; => (1 3 6 10 15 21 28 36 45 55)
; *******************************************
; Ex 3.56, Hamming
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S
  (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

; *******************************************
; *******************************************
; Ex 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(define exp1710 (expand 1 7 10))
; It's the base radix expansion of the fraction num/den.
; By Euclid's algorithm this will repeat if gcd(den, radix) != 1, e.g.:
(stream-take (expand 3 8 10) 30) ; => 
; (3 7 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

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
