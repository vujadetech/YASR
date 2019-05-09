;#lang racket

(module utils-vujadeTech racket
  (provide vujadeTech repeat powers mod negative -- ++ non-empty-list? singleton-list?
           ^ remove-once lg_ lg- alphabet
           )

  (define (vujadeTech)
    (display "Tomorrow's future ... yesterday!"))

  (define (repeat x n)
    (map (λ (_) x) (range 1 (++ n))))

  (define (powers x k) ; => '(x^0 x^1 x^2 ... x^k)
    (map (λ (k) (expt x k)) (range 0 (++ k))))




  (define mod remainder)
  (define negative (λ (x) (- 0 x)))
  (define (-- x) (- x 1))
  (define (++ x) (+ x 1))
  (define (non-empty-list? xs) (and (list? xs) (> (length xs) 0)))
  (define (singleton-list? xs) (and (non-empty-list? xs) (= (length xs) 1)))





  
  (define (remove-once x xs)
    (cond
      [(not (memq x xs)) xs]
      [(memq x (list (car xs))) (cdr xs)]
      [(cons (car xs) (remove-once x (cdr xs)))]))
  (define ^ expt)
  (define lg_ (λ (n) (inexact->exact (floor (log n 2)))))   ; floor of log base 2
  (define lg- (λ (n) (inexact->exact (ceiling (log n 2))))) ; ceiling of log base 2
  (define alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

  
  )