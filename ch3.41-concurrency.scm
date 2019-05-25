#lang racket/base
; "Thunk in the trunk!" (ok I stole that joke from the title of
; an episode of Modern Family. You know a show's funny when
; you're laughing from the title alone.)

(require "modules/utils-vujadeTech.rkt"
         "modules/stream.rkt" 
         ;racket/stream
         )

(define (parallel-execute . procs)
  (map thread-wait (map thread procs)))

(define (stream-filter-primes a b)
  (stream-car
   (stream-cdr
    (stream-filter prime?
                   (stream-enumerate-interval a b)))))

(define (test n)
  (let ([x 10])    
    (define (run)
      (define p1 (lambda ()  (set! x (* x (++ x)))))
      (define p2 (lambda () (set! x (+ x (* x (/ 1 x))))))
      ; (define p3 (λ () (sleep 0.1)(set! x (- x 1))))
      (define ps (list p1 p2))
      (apply parallel-execute ps)
      ; (display x)
      ;(apply parallel-execute (reverse ps))
      (display x)(display " "))
    
    (unless (zero? n)
      (run)
      (test (-- n))
    )
  )
  )
#;(module joke ch3.41-concurrency
  (display "Thunk in the trunk!"))

;(list->values (repeat (test) 5))
(test 10)
