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
;;(stream-take (expand 3 8 10) 30) ; => 
; (3 7 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
; *******************************************
; Ex 3.59
(define (div-streams s1 s2) ; element-wise division
  (stream-map / s1 s2))

(define harmonic-series (div-streams ones integers))

; a.
(define (integrate-series a_n)
  (mul-streams harmonic-series a_n))

; b.
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

; Show how to generate the series for sine and cosine, starting from the
; facts that the derivative of sine is cosine and the derivative of cosine is the negative of sine:

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
(define cos-series cosine-series)

; *******************************************
; Ex 3.60
(define add-series add-streams)
(define (mul-series as bs)
  (let ([a0 (stream-car as)]
        [b0 (stream-car bs)]
        [a1s (stream-cdr as)]    ; a1s = (a1 a2 a3, ... etc), and same for b1s
        [b1s (stream-cdr bs)])   
    (cons-stream (* a0 b0)
                 (add-streams (mul-series b1s as) ; rest of bs * as
                              (scale-stream a1s b0)))))

(define sin2 (mul-series sine-series sine-series))
(define cos2 (mul-series cos-series cos-series))
(define sin2+cos2 (add-series sin2 cos2))

;; (stream-take sin2+cos2 10) ; => (1 0 0 0 0 0 0 0 0 0)

; *******************************************
; Ex 3.64

(define (average x y) (/ (+ x y) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

; In our original sqrt procedure, we made these guesses be the successive values of a state variable. Instead we can generate the infinite stream of guesses, starting with an initial guess of 1:65

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (stream-limit stream tolerance) ; stop when adjacent elements are within tolerance and return 2nd one
  (let* ([a0 (stream-car stream)]
        [cdr1 (stream-cdr stream)]
        [a1 (stream-car cdr1)]
        [rest (stream-cdr cdr1)])
    (if (< (abs (- a0 a1)) tolerance)
        a1
        (stream-limit cdr1 tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
; *******************************************
; Ex 3.65
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

;(stream-take pi-stream 10)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;(stream-take (euler-transform pi-stream) 10)
(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

#;(stream-take
 (accelerated-sequence euler-transform pi-stream) 8)

(define E 2.71828)
(define ln (lambda (n)  (log n E)))

;(stream-take (pi-summands 1) 5)
(define (ln2-summands n)
  (cons-stream (/ 1.0 n) (scale-stream (ln2-summands (++ n)) -1)))
(define ln2-stream (partial-sums (ln2-summands 1)))
(define ln2-accelerated (accelerated-sequence euler-transform ln2-stream))
; The alternating harmonic sequence doesn't converge as quickly
; as pi, but then again the harmonic sequence doesn't converge at all.
; *******************************************
; Ex 3.67
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;(define xs (pairs integers integers))
; (stream-take quad1 10)
(define (pairs-all s t)
  (let ([upper (pairs s t)][lower (pairs (stream-cdr t) (stream-cdr s))])
    (interleave upper lower)))

(define cantor (pairs-all integers integers))

(define (pairs-upper s t) ; pairs but w/o the diagonal
  (pairs s (stream-cdr t)))

;(define (pairs-lower s t) (pairs-upper t s))
(define (pairs-lower s t)
  (stream-map reverse (pairs-upper s t)))

(define (diagonal s t)
  (cons-stream (list (stream-car s) (stream-car t))
               (diagonal (stream-cdr s) (stream-cdr t))))

(define upper (pairs-upper integers integers))
(define lower (pairs-lower integers integers))
(define slash (diagonal integers integers))
(define rationals (interleave slash (interleave upper lower)))
;;(stream-take rationals 20)
; => ((1 1) (1 2) (2 2) (2 1) (3 3) (1 3) (4 4) (3 1) (5 5) (2 3) (6 6) (3 2) (7 7) (1 4) (8 8) (4 1) (9 9) (2 4) (10 10) (4 2))
; *******************************************
; Ex 3.68
; Honestly this one's a bit of a brain buster.
; My nickname for Louis Reasoner is Louis "Not reasoning very well at all b/c he's hopped up on red bull after
; coding for 22 hours straight in his mother's root cellar."
#;(define (pairs s t) ; copy of real version above for comparison with Louis' bad version, pairs-68 (below).
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (pairs-68 s t) ; Louis' refactoring of SICP's perfect pairs procedure.
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))   t)
   (pairs-68 (stream-cdr s) (stream-cdr t))))
; => infinite loop
(define 68-a ((lambda (s t) (stream-map (lambda (x) (list (stream-car s) x))
               t)) integers integers))
; => also infinite loop - maybe b/c not stream-cdring on t?

; Got an idea from community.schemewiki.org/?sicp-ex-3.68:
(define (redbull s t) ; Adding a cons stream to beginning of pairs-68 will solve the non-halting behavior.
  (cons-stream
   "Louis is hopped up on red bull" ; Just need anything here to diagnose the halting/non-halting issue.
   (interleave ; This is now inside a cons stream like the original pairs proc, so it won't loop forever.
    (stream-map (lambda (x) (list (stream-car s) x))   t)
    (redbull (stream-cdr s) (stream-cdr t)))))
  ; => NOT an infinite loop. Not the correct answer either, but at least it's not loopy.

; The folks at schemewiki.org are at it again! Yup, the problem
; is that interleave is being called directly rather than being wrapped inside a cons-stream, which
; delays evaluation by default. Ladies and gentleman, one hand clapping against the other
; makes a VERY nice sound for schemewiki.org!!!

; *******************************************
; Ex 3.69

(define nats integers)

(define p1 '(1 2))(define p2 '(2 3))

(define (can-join-tuples? p1 p2) ; p1 a list of 2 elements, same for p2
  (eq? (cadr p1) (car p2)))

(define (join-tuples p1 p2)
  (if (can-join-tuples? p1 p2)
      (list (car p2) (cadr p2))
      '()))

(define (join-tuple-pair ts) ; ts = '(t0 t1)
  (join-tuples (car ts) (cadr ts)))
  
(define (join-tuple-to-stream t0 ts)
  (if (can-join-tuples? t0 (stream-car ts))
      (cons-stream (join-tuples t0 (stream-car ts))
                   (join-tuple-to-stream t0 (stream-cdr ts)))
      (join-tuple-to-stream t0 (stream-cdr ts))))

#;(define (triples s t u)
  (let ([st (pairs s t)][tu (pairs t u)])
    (let ([st0 (stream-car st)][tu0 (stream-car tu)])
      42)))

(define (tuple-tuples s t u)
  (let ([st (pairs s t)][tu (pairs t u)])
    (let ([candidates (stream-map list st tu)])
      (let ([unfiltered (stream-map join-tuple-pair candidates)]) ; might have emptys, '()
        (stream-filter (lambda (t) (not (null? t))) unfiltered)))))

(define test1 (tuple-tuples nats nats nats))

; *******************************************
; Streams as signals

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)
; *******************************************
; Ex 3.73

(define (RC R C dt) ; (current-in-stream, v0) |-> (voltage-out-stream)
  (lambda (is v0) ; is = current stream (impulse train as stream), v0 = init cap voltage
    (let ([Ri (scale-stream is R)])
      (add-streams Ri (integral is v0 dt))))) ; is = integrand

(define R1 5)(define C1 1)(define dt 0.5)
(define is1 harmonic-series)
(define v0 10)
(define RC1 (RC R1 C1 dt))
(define RC1-harmonic-10 (RC1 is1 v0))
;; (stream-take RC1-harmonic-10 6) 
; =>(15 13.0 12.416666666666666 12.166666666666666 12.041666666666666 11.975)
             
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
; *******************************************

