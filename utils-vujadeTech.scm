(module utils-vujadeTech racket
  (provide vujadeTech repeat powers
           mod negative -- ++ non-empty-list? singleton-list?
           range-fixed sq square nil 1/
           halve fst snd           ; zero?
           iter time-ms first-two-same? mean decades divides? init
           next-odd find-divisor smallest-divisor prime?
           ^ remove-once lg_ lg- alphabet
           ; Ch 3
           +=!
           )

  (define (vujadeTech)
    (display "Tomorrow's future ... yesterday!"))

  (define (repeat x n) ; kludgy, but it'll git-r-done until and if I decide to incorporate data/collections which has it built in.
    ; TODO: 
    (map (λ (_) x) (range 1 (++ n)))) 

  (define (powers x k) ; => '(x^0 x^1 x^2 ... x^k)
    (map (λ (k) (^ x k)) (range 0 (++ k))))

  (define mod remainder)
  (define negative (λ (x) (- 0 x)))
  (define (-- x) (- x 1))
  (define (++ x) (+ x 1))
  (define (non-empty-list? xs) (and (list? xs) (> (length xs) 0)))
  (define (singleton-list? xs) (and (non-empty-list? xs) (= (length xs) 1)))
  
  (define (range-fixed a b k) ; => '(a+k, a+2k, ... , b-k, b)
    (let ([step (/ (- b a) k)])
      (range a (+ b step) step)))
  (define (sq x) (* x x))
  (define square sq)
  (define nil null)
  (define 1/ (λ (x) (/ 1 x)))

  (define (halve x) (/ x 2))
;  (define (double x) (* x 2)) 
  (define (fst xs) (car xs))
  (define (snd xs) (cadr xs))
;  (define (zero? x) (= x 0)) ; This is already a racket built in.
  (define (iter f k n)
    (cond [(zero? k) n]
          [(= 1 k) (f n)]
          [else (f (iter f (-- k) n))]))
  (define (time-ms proc . xs) ; time in ms of running proc on xs, return pair (result . "time in ms") 
    (let* ([t0 (current-inexact-milliseconds)] [y (apply proc xs)] [delta (- (current-inexact-milliseconds) t0)])
      (cons y delta)))
  (define (first-two-same? x . xs) ; example of dotted notation
    (if (= x (car xs)) #t #f))
  (define (mean xs) (/ (apply + xs) (length xs))) ; arithmetic mean
  (define (decades n) (map (lambda (n) (expt 10 n)) (range 0 n))) ; useful for checking O(f(n)) growth, e.g. in prime testing problems
  (define (divides? a b) (= (remainder b a) 0))
  (define (init xs) ; assume xs non-empty since init is meaningless otherwise
    (if (empty? (cdr xs))
        '()
        (cons (car xs) (init (cdr xs)))))

  (define (next-odd n) (+ n 1 (mod n 2))) ; add an extra 1 if n is odd (i.e., n (mod 2) = 1)

  (define (find-divisor+ n test-divisor) ; refactor of find-divisor; '+' b/c it will be an improvement, or at least one hopes.
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor+ n (next-odd test-divisor)))))
  (define (smallest-divisor+ n) ; refactor of smallest-divisor
    (find-divisor+ n 2))
  (define (prime?+ n) (= (smallest-divisor+ n) n)) ; improved prime? proc
  (define find-divisor find-divisor+)
  (define smallest-divisor smallest-divisor+)
  (define prime? prime?+)
  
  (define (remove-once x xs)
    (cond
      [(not (memq x xs)) xs]
      [(memq x (list (car xs))) (cdr xs)]
      [(cons (car xs) (remove-once x (cdr xs)))]))
  (define ^ expt)
  (define lg_ (λ (n) (inexact->exact (floor (log n 2)))))   ; floor of log base 2
  (define lg- (λ (n) (inexact->exact (ceiling (log n 2))))) ; ceiling of log base 2
  (define alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

  ; Chapter 3:
  (define (+=! i bump) ; Ripoff of C++ i += bump. Since we appear to be going C++/Java, we may as well
    ; use their established and succinct idioms. Add ! to indicate it's destructive.
    (set! i (+ i bump))
    i) ; This didn't work; it appears to be passing by value, which would make sense given
  ; that Scheme is a nearly pure functional language. Of course it's annoying here when
  ; some old school state manipulation using pass by ref would come in handy, but oh Scheme, I can't stay mad at you!!!

 ; (define (do f x k)
  
)