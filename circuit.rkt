#lang racket
;(require "queue.rkt")
(require "modules/agenda.rkt") ; agenda.rkt includes submodule queue.rkt.
(require "utils-vujadeTech.rkt")
;(require r5rs) ; for set-car! and related

; GLOBALS, brought to you by Vuja de Tech: "Tomorrow's future ... yesterday!"
; Also with a little help from my friends at SICP.
; [MIT SICP mini-commercial]: Do you like computer science? And do you like books?
; Then you're gonna LOOOOOOOOOVE the computer science book "Structure and Interpretation of Computer Programs"!
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)
(define nor-gate-delay (+ inverter-delay or-gate-delay))
(define gated-d-latch-delay (+ inverter-delay and-gate-delay nor-gate-delay))
(define DFF-delay 1)
; END GLOBALS

(define (make-wire)
  (let ([signal-value 0] [action-procedures '()])
    
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures)) ;change value of signal will lead to the call of every procedures added to the wire
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))
; END make-wire function
; make-wire related:
(define (call-each procedures) ; used in make-wire
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire))
                 (newline)
                 )))
; I'm changing SICP's semantics slightly. A wire is a pair of a label and
; what I'll now call the "bare-metal-wire" (SICP wire) b/c it's funny and I needed a different name.
(define (make-labelled-wire label) (cons label (make-wire)))
(define (make-labelled-wires labels) (map make-labelled-wire labels))
(define (get-label wire) (car wire))
(define (get-labels ws) (map get-label ws))
(define (get-bare-metal wire) (cdr wire))
(define (get-bare-metals ws) (map get-bare-metal ws))
(define (make-bare-metals n) (repeat (make-wire) n))
; ***********************************************
; get-signal code (basic version already set from sicp code)
(define (get-signal* w*) (cons (get-label w*) (get-signal (get-bare-metal w*))))
(define (get-signals ws) ; ws = a list of bare-metal-wires, no labels
  (map get-signal ws))
#;(define (get-signals* ws*) ; returns list of pairs '((label0, x0) (label1, x1)...)
  (map duple-to-pair (zip (get-labels ws*) (get-signals (get-bare-metals ws*)))))
(define (get-signals* ws*) (map get-signal* ws*))

(define (get-nth-signal* ws* n) ; 0 based, wires are pairs with labels
  (let* ([pn (nth ws* n)][label-n (get-label pn)][wire-n (get-bare-metal pn)])
    (cons label-n (get-signal wire-n))))

(define (get-nth-wire* ws* n) (nth ws* n))
; ***********************************************
; set-signal code (basic version already set from sicp code)
(define (flip-signal! w) (set-signal! w (logical-not (get-signal w)))) ; added for clock but could be more generally useful
(define (set-signal*! w* x) ; w = wire, x = new signal value
  (set-signal! (get-bare-metal w*) x))
(define (set-signals! bare-metals xs)
  (let ([wires-values (zip bare-metals xs)])
    (map (λ (wv) (set-signal! (car wv) (cadr wv))) wires-values)))
(define (set-signals*! ws* xs) (map set-signal*! ws* xs))
; ***********************************************

; Logic 'R Us (a subsidiary of "Literacy 'R Us").
; Please don't tell "Toys 'R Us" lest they sue for copyright infringement.
; TODO-laffs: Clip of "Toys 'L Us" from simpsons

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-or s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
        ((or (= s1 1) (= s2 1)) 1)
        (else (error "Invalid signal" s1 s2))))

(define (logical-nor s1 s2) (logical-not (logical-or s1 s2)))

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((or (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s1 s2))))

; Bill's gates (not to be confused with Bill Gates, the fact that
; the gates belong to a man named "Bill" is just a coincidence).

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value))))) ;output signal changes after one inverter-delay
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
          (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
        and-gate-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay
       or-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (nor-gate a1 a2 output)
  (define (nor-action-procedure)
    (let ((new-value
           (logical-nor (get-signal a1) (get-signal a2))))
      (after-delay
       (+ or-gate-delay inverter-delay)
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 nor-action-procedure)
  (add-action! a2 nor-action-procedure)
  'ok)

; Derived gates (using Bill's gates as a basis), so I don't have to
; call the add-action! procedure :D

#;(define (nor nor1 nor2 out) ; nor gate implemented as an or gate into an inverter
  (let ([or-out (make-wire)])
    (or-gate nor1 nor2 or-out)
    (inverter or-out out))
    'nor-ok)

(define (nand a1 a2 out)
  (let ([and-out (make-wire)])
    (and-gate a1 a2 and-out)
    (inverter and-out out))
    'nand-ok)

(define (xor in1 in2 out) 
  (let ([xor-out (make-wire)][not-in1 (make-wire)][not-in2 (make-wire)][and1-out (make-wire)][and2-out (make-wire)])
    (inverter in2 not-in2) (inverter in1 not-in1)
    (and in1 not-in2 and1-out)
    (and in2 not-in1 and2-out)
    (or-gate and1-out and2-out out))
    'xor-ok)

#;(define (sr-latch s r q) ; sr latch with just 1 output, q
  (let ([q_ (make-wire)])  ; q_ is not-q
    (nor-gate s q q_)
    (nor-gate r q_ q))
  'sr-latch-ok)

(define (sr-latch-direct s r q) ;
0)
  ;(define (sr-action-procedure) 0))

#; (define (gated-d-latch d e q) ; based on wikipedia SR NOR latch: wiki/Flip-flop_(electronics)
  (let ([d-not (make-wire)]
       ; [d-e-and-out (make-wire)] ; 
        [s (make-wire)][r (make-wire)])
    (inverter d d-not)
    (and-gate d-not e r)
    (and-gate d e s)
    (sr-latch s r q))
     'gated-d-latch-ok)

(define (DFF in clock out) ; direct implementation of data flip-flop
  ; from Elements of Comp Sys book.
  ; [Joke break]: How do you know you've been spending too much time
  ; in your electronics lab? When you're DFF is a BFF. HEY-oh! 
  (define (DFF-action-procedure)
    (let ([Q-now (get-signal out)])
      (let ([Q-next (if (zero? (get-signal clock)) Q-now (get-signal in))])
        (after-delay DFF-delay (λ () (set-signal! out Q-next))))))
  (add-action! in DFF-action-procedure)
  (add-action! clock DFF-action-procedure) ; clock is acting like an input, so it needs and action proc  
  'DFF-ok)

(define (gated-d-latch d e q) ; direct version using add-action!
  (define (gated-d-latch-act-proc)
    (let ([d-sig (get-signal d)][e-sig (get-signal e)][q-sig (get-signal q)])
      (let ([q-new (if (zero? e-sig) q-sig ; hold current value
                       d-sig)]) ; q-sig(n+1) = d-sig(n)
        (after-delay gated-d-latch-delay (λ () (set-signal! q q-new))))))
  (add-action! d gated-d-latch-act-proc)
  (add-action! e gated-d-latch-act-proc)
  'gated-d-latch-ok)
  
(define (nand+ as out) ; as is a list of bare-metals.
  (let-values ([(a0 a1) (list->values as)])
   (nand a0 a1 out)
    ))

(define nand+in0 (make-wire))
(define nand+in1 (make-wire))
(define nand+out (make-wire))
(define nand+ins (make-bare-metals 2))
;(define Nand (nand+ (list nand+in0 nand+in1) nand+out)) ; worked
(define Nand (nand+ nand+ins nand+out)) ; worked

(define (nand* as* out*)
  (let-values
      ([(a0 a1 out) (list->values (get-bare-metals (append as* (list out*))))])
    (nand a0 a1 out))
    'nand-*ok)

(define (half-adder a b s c) ;input: a,b output:s,c
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d) ;d=a+b
    (and-gate a b c) ;c=ab
    (inverter c e) ;e=c'=(ab)'=a'+b'
    (and-gate d e s) ;s=ed=(a+b)(a'+b')=ab'+a'b
    'ok))
  
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1); s=b(c-in)'+b'(c-in) c1=b(c_in)
    (half-adder a s sum c2)
    ;sum=as'+a's=a(b(c-in)+b'(c-in)')+a'(b(c-in)'+b'(c-in))
    ;sum=ab(c-in)+ab'(c-in)'+a'b(c-in)'+a'b'(c-in)
    ;c2=as=ab(c-in)'+ab'(c-in)
    (or-gate c1 c2 c-out)
    ;c-out=c1+c2=b(c_in)+ab(c-in)'+ab'(c-in)
    ;c-out=b(c_in)+(ab(c_in)+ab(c-in)')+(ab(c_in)+ab'(c-in))
    ;c-out=b(c_in)+ab+a(c_in)
    'ok))

#;(define (test-nand xs) ; xs is a string of binary numbers as input
  (let* ([as* (make-wires '(a0 a1))][out* (make-labelled-wire 'out)][out (get-bare-metal out*)])
    (let*-values
        ([(a0* a1*) (list->values as*)]
         [(a0  a1 ) (list->values (get-bare-metals as*))])
  ;   (display a0) 
     (set-signal! a0 (nth xs 0))
     (set-signal! a1 (nth xs 1))
     (nand a0 a1 out)
     (propagate)
   (get-signal out))))

(define (test-nand xs) ; xs is a string of binary numbers as input
  (let* ([as* (make-labelled-wires '(a0 a1))][out* (make-labelled-wire 'out)][out (get-bare-metal out*)])
    (let*-values
        ([(a0* a1*) (list->values as*)]
         [(a0  a1 ) (list->values (get-bare-metals as*))])
  ;   (display a0) 
     (set-signal! a0 (nth xs 0))
     (set-signal! a1 (nth xs 1))
     (nand a0 a1 out)
     (propagate)
   (get-signal out))))

(define generic-nand (nand (make-wire) (make-wire) (make-wire)))

(define (test-gate gate xs num-outs) ; xs = list of binary values, n-outs is the number of outputs - single integer
  (let* ([outs (make-bare-metals num-outs)]
         [num-ins (length xs)]
         [ins (make-bare-metals num-ins)]
         ;[wires-values (zip ins xs)]
         )
    (unless (> num-outs 1) (set! outs (car outs))) ; list not needed for a single output
    (set-signals! ins xs) ; set input signals
    (gate ins outs) ; make gate
    (propagate)     ; run gate
    (display (list outs num-ins ins))(newline)
    (display (get-signal outs))
    )
  'test-gate-ok)
;(test-gate (generic-nand+ '(0 0) 1))

; CLOCK

(define clock-out (make-wire))
(probe 'clock-out  clock-out)
(define and0 (make-wire))
;(probe 'and0 and0)
;(set-signal! and0 1)
;(define and-out (make-wire))
;(probe 'and-out and-out)
;(and-gate and0 clock-out and-out)

(define (make-clock out) ; Connect wire out to the clock output
  (let ([cycle-count (make-accumulator 0)])
    (define (get-cycle-count) (display "Cycle count: ") (cycle-count 'get))
    (define advance1 ; advance 1 clock cycle
      (λ ()
        (display (cycle-count 'get))
        (cycle-count '++)
        (flip-signal! out)
        (propagate) ; propagate clock change to other gates
        ))
    (define advance ; advance k clock cycles
      (λ (k) ; k > 0
        (cond ; using cond instead of if since else has 2 steps and I'm too lazy
          ; to type "begin", although I just did. :D
          [(= k 1) (advance1)]
          [else (advance1) (advance (-- k))])))
    (define (clock-is-low?) (zero? (get-signal out)))
    (define (dispatch m)
      (cond
        [(eq? m 'get-signal) (get-signal out)]
        [(eq? m 'get-cycle-count) (get-cycle-count)]
        [(eq? m 'advance1) (advance1)]
        [(eq? m 'advance)   advance]
        [(eq? m 'clock-is-low?) (clock-is-low?)]
        [(eq? m 'clock-is-high?) (not (clock-is-low?))]
        [else 'error-dispatch-make-clock]))
    dispatch))
(define clock (make-clock clock-out))
               
(define (clock-h freq-Hz out max-cycles) ;
  (let ([flip-delay (/ 1 freq-Hz)][cycle-acc (make-accumulator 0)])
    (sleep flip-delay)
    (set-signal! out (logical-not (get-signal out)))
    (cycle-acc '++)
    (display (cycle-acc 'get))
    (unless (> (cycle-acc 'get) max-cycles)
      (clock-h freq-Hz out max-cycles))))

; TESTING AREA
;(test-gate nand+ '(0 0) 1)
(define nand*-ins (make-labelled-wires '(ni1 ni2)))
(define as (make-labelled-wires '(a0 a1)))
(define bs (make-labelled-wires '(b0 b1)))
(define nand*-out (make-labelled-wire 'nand*-out))
(define c-in (make-wire))
(define sum0 (make-wire))
(define sum1 (make-wire))
(define c-out (make-wire))
;(define generic-nand* (nand* (make-labelled-wires '(a0 a1)) (make-labelled-wire 'out))) ; (nand* nand*-ins nand*-out))

; Test DFF
(define din (make-wire))(define dout (make-wire))
(probe 'din din)
(probe 'dout dout)
(DFF din clock-out dout)

; Test sr-latch
(define r (make-wire))(define s (make-wire))
(define q (make-wire))
(probe 'q q)
;(sr-latch s r q)
;(define q_ (make-wire))(set-signal! q_ 1) ; q_ must be opposite
;(sr-latch r s q q_)
;(sr1 s r q)
;(sr1b s r q)
; Test d flip flop
(define d (make-wire))(define e (make-wire))
(define Q (make-wire))
(probe 'Q Q)
;(gated-d-latch d e Q)

(define (ripple-adder-2 as bs c-in sum0 sum1 c-out)
  (let ([c0 (make-wire)][a-wires (get-bare-metals as)][b-wires (get-bare-metals bs)])
    (let ([a0 (nth a-wires 0)][a1 (nth a-wires 1)][b0 (nth b-wires 0)][b1 (nth b-wires 1)])
      (full-adder a0 b0 c-in sum0 c0)
      (full-adder a1 b1 c0   sum1 c-out)) ; end let-values
    'ripple2-ok))

;(ripple-adder-2 as bs c-in sum0 sum1 c-out)
(define (ripple-adder-2* as* bs* cs* sums*) ; * means it's expecting a (label . wire) pair, not just a bare-metal wire.
  (let ([as (get-bare-metals as*)][bs (get-bare-metals bs*)][cs (get-bare-metals cs*)][sums (get-bare-metals sums*)])
    (let-values
        ([(a0 a1) (list->values as)]
         [(b0 b1) (list->values bs)]
         [(c-in cout) (list->values cs)]
         [(sum0 sum1) (list->values sums)])
      (list a0 a1 b0 b1 c-in sum0))))
    
#;(define (ripple-adder-2 as bs c-in sum0 sum1 c-out)
  (let ([c0 (make-wire)][a-wires (get-bare-metals as)][b-wires (get-bare-metals bs)])
    (let-values ([(a0 a1) a-wires][(b0 b1) b-wires])
      (full-adder a0 b0 c-in sum0 c0)
      (full-adder a1 b1 c0   sum1 c-out)) ; end let-values
    'ripple2-ok))
        
;(define (ripple-add-2 
;(define (wire-array ws) ; 
  

; TEST:



(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
;(probe 'sum sum)
;sum 0  New-value = 0
;(probe 'carry carry)
;carry 0  New-value = 0

; Next we connect the wires in a half-adder circuit (as in figure 3.25), set the signal on input-1 to 1, and run the simulation:

;(half-adder input-1 input-2 sum carry)
;ok
;(set-signal! input-1 1)
;done
;(propagate)
;sum 8  New-value = 1
;done

;The sum signal changes to 1 at time 8. We are now eight time units from the beginning of the simulation. At this point, we can set the signal on input-2 to 1 and allow the values to propagate:

;(set-signal! input-2 1)
;done
;(propagate)
;carry 11  New-value = 1
;sum 16  New-value = 0
;done

; Test nand:
(define i1 (make-wire))
(define i2 (make-wire))
;(define nand-out (make-wire))
;(probe 'nand-out nand-out)
;(nand i1 i2 nand-out)