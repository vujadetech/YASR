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
(stream-prime 10000 100000000000000) ; => 10009

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
(define (take-stream stream k) ; make a normal list up to k elements of a stream
  (if (zero? k) '()
      (cons (stream-car stream) (take-stream (stream-cdr stream) (-- k)))))

(define add-nats (stream-map + nats-to-million nats-to-million))
;add-nats ; => (2 . #<procedure:...-stream-sicp.scm:27:4>)
;(take-stream add-nats 10) ; => (2 4 6 8 10 12 14 16 18 20)

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
