#lang sicp
; (#%require racket/base)
(#%require "utils-vujadeTech.scm")

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

; Construct the "never return at all" with z-cycle in parts: Z1 and Z2.
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

; Abstract count-pairs to arbitrary proc, call it map-cell (cell is either a pair or just an atom like a num or symbol).
; TODO
#;(define (map-pair proc q ) ; q could be a pair
  (let ([acc nil])
    ()))


; *******************************************
; 3.18
(define count-pairs count-pairs-vujadeTech)

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

(define (has-cycle? v)
  (or (can-reach-itself? v) (has-cycle? (car v)) (has-cycle? (cdr v))))

(define w3 '(a b c))
(make-cycle w3)

; *******************************************
; Ex 3.19 TODO
(define (car-or-cdr-is-pair? p) ; assume p is a pair, => #t iff (cdr p) is a pair (car p) is a pair
  (or (pair? (car p)) (pair? (cdr p))))

(define (terminal-node? p) ; either p isn't a pair, or it is but neither it's car nor cdr is a pair.
  (or (not (pair? p)) (not (car-or-cdr-is-pair? p))))

(define (non-terminal-node? p) (not (terminal-node? p)))

(define (delete-leafs p) ; delete terminal-nodes from p, or leafs for short
  (let ([acc '()])
    (define (delete-help p)
      (cond
        [(not (pair? p)) p]
      ;  [(memq q acc) 0] ; already accounted for
        [(terminal-node? p) p] ; p is a pair but it's terminal, so neither car nor cdr are pairs.
        ; So now p has a pair in the car or cdr (or both).
        [(terminal-node? (car p)) (cons p (delete-leafs (cdr p)))] ;  (set-car! p nil)]
        [(terminal-node? (cdr p)) (set-cdr! p nil)]
        [else (error "delete-leafs")]))
    (delete-help p)))

(define (delete-leafs2 p) ; delete terminal-nodes from p, or leafs for short
  (let ([acc '()])
    (define (delete-help p)
      (cond
        [(or (not (pair? p)) (terminal-node? p)) acc]
      ;  [(memq q acc) 0] ; already accounted for
    ;    [(terminal-node? p) p] ; p is a pair but it's terminal, so neither car nor cdr are pairs.
        ; So now p has a pair in the car or cdr (or both).
   ;     [(terminal-node? (car p)) (cons p (delete-leafs (cdr p)))] ;  (set-car! p nil)]
   ;     [(terminal-node? (cdr p)) (set-cdr! p nil)]
        [else
         (set! acc (cons p acc))
         (set-cdr! p (delete-leafs2 (cdr p)))
         acc]))
    (if (terminal-node? p)
        p
        (delete-help p))))

;(define (prune x) (set! x nil))
(define (prune-cdr x) (set-cdr! x nil))
(define (prune-car x) (set-car! x nil))

; *******************************************
; Ex 3.20 NA
; *******************************************
; 3.3.2  Representing Queues
; Is it just me or does the word "queues" have too many vowels?

; ADT:
(define (make-queue) (cons nil nil))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define empty-q? empty-queue?)
; (front-queue <queue>)
; returns the object at the front of the queue, signaling an error if the queue is empty; it does not modify the queue.
(define (front-queue queue)
  (if (empty-q? queue)
      (error "front-queue: Q is empty" queue)
      (car (front-ptr queue))))
(define front-q front-queue)
; (insert-queue! <queue> <item>)
; inserts the item at the rear of the queue and returns the modified queue as its value.
(define (insert-queue! q item)
  (let ([new-pair (cons item '())])
    (cond ; 2 cases, q empty or not:
      [(empty-q? q) (set-front-ptr! q new-pair)
                    (set-rear-ptr!  q new-pair)
                    q]
      [else (set-cdr! (rear-ptr q) new-pair)
            (set-rear-ptr! q new-pair)
            q])))
(define insert-q! insert-queue!)

; (delete-queue! <queue>)
; removes the item at the front of the queue and returns the modified queue as its value, signaling an error if the queue is empty before the deletion.
(define (delete-queue! q)
  (if (empty-q? q)
      (error "Delete called on empty q, " q)
      (set-front-ptr! q (cdr (front-ptr q))))
  q)
(define delete-q! delete-queue!)

; pointer implementation:
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr!  queue item) (set-cdr! queue item))

; Example q
(define q (make-queue))
(insert-queue! q 'a)	
(insert-queue! q 'b)	
;(delete-queue! q)	
(insert-queue! q 'c)	
(insert-queue! q 'd)
;(delete-queue! q)

; *******************************************
; Ex 3.21
#;(define (print-queue q)
  (cond
    [(empty-q? q) (display nil)]
    [else (display (front-q q))
          (set-front-ptr! q (cdr (front-ptr q)))
          (display q)]))

(define (print-queue q) ; Just display the front-ptr since it has the list of all items.
    (display (front-ptr q))
    (newline))
(define print-q print-queue)
      
; *******************************************
; Ex 3.22
; A queue is represented, then, as a pair of pointers, front-ptr
; and rear-ptr, which indicate, respectively, the first and
; last pairs in an ordinary list. 
(define (make-Q)
  (let ([front-ptr nil][rear-ptr nil]) ; [q (cons front-ptr rear-ptr)])
    (define (get-q) (cons front-ptr rear-ptr))
    (define (empty-q?) (null? front-ptr))
    (define (front-q)
      (if (empty-q?) (error "empty Q, no front")
          (car front-ptr)))
    (define (insert-q! item)
      (let ([new-pair (cons item nil)])
        (cond
          [(empty-q?) (set! front-ptr new-pair)
                      (set! rear-ptr  new-pair)
                      (get-q)]
          [else (set-cdr! rear-ptr new-pair)
                (set! rear-ptr new-pair)
                (get-q)])))
    (define (delete-q!)
      (cond
        [(empty-q?) (error "delete-q! in make-Q")]
        [else
         (set! front-ptr (cdr front-ptr))
         (get-q)] ; 
      )) ; end define delete-q!
    (define (print-q) (display front-ptr) (newline))
            
    (define (dispatch m)
      (cond
        [(eq? m 'empty-q?) (empty-q?)]
        [(eq? m 'front-q)   (front-q)]
        [(eq? m 'insert-q!) insert-q!]
        [(eq? m 'delete-q!) (delete-q!)]
        [(eq? m 'print-q)   (print-q)]
        [else (error "dispatch of make-Q")])
      ) ; end define dispatch
    dispatch))

(define q22 (make-Q))
((q22 'insert-q!) 1)
((q22 'insert-q!) 2)
(q22 'print-q)
(q22 'delete-q!)
  
; *******************************************
; Ex 3.23, Deque
; Leverage make-queue so you DRY.
(define (make-deque) (make-queue))
(define (empty-deque? deque) (empty-queue? deque))   (define empty-dq? empty-deque?)
(define (front-deque deque)  (front-queue  deque))   (define front-dq front-deque)
(define (rear-deque deque)
  (if (empty-dq? deque)
      (error "deque empty" deque)
      (car (rear-ptr-dq deque))))
(define rear-insert-deque! insert-queue!)
(define (front-insert-deque! deque item)
  (let ([new-pair (cons item nil)])
    (cond
      [(empty-dq? deque) (rear-insert-deque! deque item)]
      [else (set-cdr! new-pair (front-ptr-dq deque))
            (set-front-ptr-dq! deque new-pair)
            deque])))
(define front-delete-deque! delete-queue!)

(define (delete-last! xs) ; assume xs non-empty. Delete last and return ptr to the new last.
  (let ([next-to-last xs]) ; may not need this var
    (define (del-last-h xs ptr)
      (cond
        [(null? (cddr ptr)) (set-cdr! ptr nil)
                             ptr]
        [else (set! ptr (cdr ptr))
              (del-last-h xs ptr)
              ]
        )) ; del-last-h
    (if (null? (cdr xs)) 
        (begin
          (set! xs nil)
          (display xs)
          )
        (del-last-h xs xs)) ; end if
    ))

(define (rear-delete-deque! deque) ; This is O(n) rather than O(1).
  (cond
    [(empty-dq? deque) (error "rear-delete")]
    [(null? (cdr (front-ptr-dq deque))) ; could also just make-queue here
     (set-front-ptr-dq! deque nil)
     (set-rear-ptr-dq!  deque nil)
     deque]
    [else
     (set-rear-ptr-dq! deque (delete-last! (front-ptr-dq deque)))]))
    
  
(define print-dq print-q)

; pointer implementation:
(define front-ptr-dq front-ptr)
(define rear-ptr-dq  rear-ptr )
(define set-front-ptr-dq! set-front-ptr!)
(define set-rear-ptr-dq!  set-rear-ptr!)
(define front-delete-dq!  delete-q!)

(display "DQ")(newline)
(define dq (make-deque))
(print-dq dq)
(front-insert-deque! dq 3)
(print-dq dq)
(rear-insert-deque! dq 1)
(print-dq dq)
(front-insert-deque! dq 2)
(print-dq dq)
(front-delete-deque! dq)
(print-dq dq)

; *******************************************
; Representing Tables

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))


(define (assoc key records); [pred equal?])
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

; To insert a value in a table under a specified key, we first use assoc to see if there is already a record in the table with this key. If not, we form a new record by consing the key with the value, and insert this at the head of the table's list of records, after the dummy record. If there already is a record with this key, we set the cdr of this record to the designated new value. The header of the table provides us with a fixed location to modify in order to insert the new record.25

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

; To construct a new table, we simply create a list containing the symbol *table*:

(define (make-table) (list '*table*))

(define t (make-table))
(insert! 'a 1 t)
;(insert! 'b 2 t)
;(insert! 'c 3 t)
(assoc 'a (cdr t))

; *******************************************
; Two-dimensional tables

; When we look up an item, we use the first key to identify the
; correct subtable. Then we use the second key to identify the
; record within the subtable.

(define (lookup-2d key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert-2d! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

(define t2d (make-table))
(insert-2d! 'trump 'hair "Eagles soar!" t2d)
(insert-2d! 'trump 'golf "Best in world" t2d)
(insert-2d! 'car 'smell "Stinky!" t2d)
;(insert-2d! 'car 'size  "too small" t2d)
;(insert-2d! 'car 'steering-wheel  "A GREAT steering wheel that won't WHIFF out the window while I driving." t2d)
;(insert-2d! 'car 'why-no-good-ideas? "B/c Paul keep farting!" t2d)
(insert-2d! 'paul 'wife "Mother in law!" t2d)
;(lookup-2d  'car 'size t2d)
;(insert-2d! 'trump 'size "YUUUGE!" t2d)


; *******************************************
; Creating local tables

(define (make-table-local)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'get-table) local-table)
            (else (error "Unknown operation -- TABLE" m))
            ))
    
    dispatch))

; Using make-table, we could implement the get and put operations used in section 2.4.3 for data-directed programming, as follows:

(define operation-table (make-table-local))
(define opt operation-table)
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define get-table (opt 'get-table))

(put 'a "b" 1)


; *******************************************
; Ex 3.24

(define (assoc-pred key records pred?)
  (cond ((null? records) false)
        ((pred? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert-pred! pred? table) ; 
  (lambda (key value) ; table not needed since it's wrapped in a table object
    (let ((record (assoc-pred key (cdr table) pred?)))
      (if record
          (set-cdr! record value)
          (set-cdr! table
                    (cons (cons key value) (cdr table)))))
    'ok))

(define (lookup-pred pred? table)
  (lambda (key)
    (let ((record (assoc-pred key (cdr table) pred?)))
      (if record
          (cdr record)
          false))))

(define (make-table-g same-key?) ; Generic version of make-table
  (let ([T (make-table)])
    ; member functions
    
    ; dispatch
    (define (dispatch m)
      (cond
        [(eq? m 'get-table) T]
        [(eq? m 'insert!) (insert-pred! same-key? T)]
        [(eq? m 'lookup)  (lookup-pred  same-key? T)]
        
        [else (error "Dispatch make-table-g")]
        ))
  dispatch))

(define (equal-ish? tolerance)
  (lambda (x y) (<= (abs (- x y)) tolerance)))

(define tg (make-table-g equal?)) ; table generic
;;((tg 'insert!) 'a 1)

(define tg2 (make-table-g (equal-ish? 3)))
((tg2 'insert!) 5 'a)
;;((tg2 'lookup) 8) ; => 'a since it's in tolerance of 3
;;((tg2 'lookup) 9) ; => #f, outside the tolerance

; *******************************************
; Ex 3.25 TODO
; One can use a normal table and store the list as the key for the value.
; Not sure what this one is asking for.

(insert! '(trump hair) "eagles soar" t)
(insert! '(car smell)  "stinky" t)
;; (lookup '(car smell) t) ; => "stinky"

; yccsml.com version, refactor to use lookup from SICP.
#;(define (insert*! ks value table) ; ks = non-empty keylist
  (if (null? (cdr ks))
      (insert! (car ks) value table)
      0))
      
; Meta-table: a table of tables; table-name-val = (pair table-name value), value=nil means unassigned
; Better than a nosql database :D
(define TABLE-LABEL '*table*) ; Might want to change this later to *meta-table*.
(define VALUE-LABEL '*val*)
;(define (make-meta-table table-name table-value) ; e.g. if inserting 'a 1, table-name is 'a.
 ; (list (list '*table* table-name table-value)))
 ; (cons (list '*table* table-name) table-value))
;(define root (make-meta-table 'root 'table))
;(define (get-table-name table) (cadar table))
;(define (get-table-value table) (cddar table))
;(get-table-name root)

(define (make-meta-table table-name)   
  (list (cons '*table* table-name))) ; still calling it *table* for simplicity,
; but all *table*s are in fact meta-tables.

;(cons (list '*table* table-name) table-value))
(define troot (make-meta-table 'root))
(define ta (make-meta-table 'a))
(insert! VALUE-LABEL 1 ta)
(insert! ta nil troot)
(define tab (make-meta-table 'ab))
(insert! VALUE-LABEL 2 tab)
(insert! tab nil ta)
;(insert! tb nil troot)
(define tc (make-meta-table 'c))
(insert! VALUE-LABEL 3 tc)
(insert! tc nil troot)

(lookup '((TABLE-LABEL . a) (VALUE-LABEL . 1)) troot) ; => '()

;(define (get-table-name table) (cdar table))
(define (get-table-name table) (cdr (assoc TABLE-LABEL table)))

; Meta-tables have 2 ways to lookup: a value or a meta-subtable.
(define (lookup-value table) ; only one key possible: VALUE-LABEL. Table must be the correct subtable.
  (lookup VALUE-LABEL table))
#;(define (lookup-subtable sub-name table) ; sub-name is just the atom, like 'a, not '(TABLE-LABEL, a)
; (let ([key (cons TABLE-LABEL sub-name)])
    (let ([record (assoc-meta sub-name (cdr table))])
      (if record
          (cdr record) ; or (cdr record)?
          #f)))
(define (lookup-subtable sub-name table)
  (assoc-meta sub-name table))

(define (records table) (cdr table))

#;(define (lookup-meta key table) ; returns the value if key = VALUE-LABEL and the subtable o.w.
  (if (eq? key VALUE-LABEL)
      (lookup-value table)
      (lookup-subtable key table)))

(define (lookup-meta key table) ; returns the value or the subtable if key != VALUE-LABEL
  (if (eq? key VALUE-LABEL)
      (lookup-value table)
      (assoc-meta key (cdr table))))

(define (lookup* keys table) ; keys is the list of keys, assume non-empty
  (let* ([k1 (car keys)][t1 (lookup-meta k1 table)])
    (cond
      ; If only 1 key, then looking up the value of the table itself
      [(null? (cdr keys)) (lookup-meta VALUE-LABEL t1)]  ; lookup-meta?  (lookup-value (lookup-subtable (car k1)))]
      [else (lookup* (cdr keys) t1)])))
  

(assoc '(*table* . a) (cadr troot)) ; => ((*table* . a) (*val* . 1))
(define meta-records (cdr troot))

; To assoc to value, key=VALUE-LABEL and records is the subtable with the needed records.
; Else it will return the subtable records.
(define (assoc-meta key records) ; key = value, call normal assoc, else assoc-sub
  (if (eq? key VALUE-LABEL) ; This check may not be necessary b/c of lookup-value
      (assoc key records)
      (assoc-meta-sublabel (cons TABLE-LABEL key) records)))

; passes in sublabel
(define (assoc-meta-sublabel sublabel meta-records)
  (if (null? meta-records)
      #f
      (let ([first-assoc (assoc sublabel (car meta-records))])
        (cond
          [first-assoc first-assoc]
          [else (assoc-meta-sublabel sublabel (cdr meta-records))]))))

(define (make-table-label table-name) (cons TABLE-LABEL table-name))
;(define (is-table?) (
;  (assoc '(*table* . c) (car records)) => table
(define (assoc-sub sub-label records) ; get entire subtable from records
  ; sub-name is just the atom, like 'a. sub-label=(TABLE-LABEL . 'a)
;  (let ([sub-label (cons TABLE-LABEL sub-name)])
    (assoc sub-label (car records)))

(define (get-subtable sub-label records)
  (let ([first-subtable (caar records)])
    (assoc sub-label records)))

;(get-table-name table-root)
;(define (get-table-value table) (cddar table))

(define (insert*! ks value meta-table)
  (let ([key1 (car ks)])
    (cond
      [(null? (cdr ks)) (insert! key1 value meta-table)]
      [else 0])))

;(define m1 '((*table* root table) ((*table* 'a 1))))



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