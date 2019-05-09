#lang racket
(require srfi/1) ; for zip
(require "utils-vujadeTech.scm")

; From SICP, Huffman code section:
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

; If we make a tree in this way, we have the following selectors:

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; The following procedure implements the decoding algorithm. It takes as arguments a list of zeros and ones, together with a Huffman tree.

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; The following procedure takes a list of symbol-frequency pairs such as ((A 4) (B 2) (C 1) (D 1))
; and constructs an initial ordered set of leaves, ready to be merged according to the Huffman algorithm:

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

; *******************************************
; Ex 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define st sample-tree)

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; (decode sample-message sample-tree) ; => '(A D A B B C A)
; *******************************************
; Ex 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

#;(define (encode-symbol-h symbol tree) ; symbol is assumed to be in tree for the helper function
  (unless (leaf? tree) ; if it's a leaf do nothing since done
    (cond
      [(has-leaf-sym? (left-branch tree) symbol)
       (if (leaf? (left-branch tree))
           '(0)
           (cons 0 (encode-symbol-h symbol (left-branch tree))))]
      [else
       (if (leaf? (right-branch tree))
           '(1)
           (cons 1 (encode-symbol-h symbol (right-branch tree))))])))

; Refactored version:
(define (encode-symbol-h symbol tree)
  (unless (leaf? tree)
    (let* ([bit (if (has-leaf-sym? (left-branch tree) symbol) 0 1)]
           [branch (if (= bit 0) (left-branch tree) (right-branch tree))])
      (if (leaf? branch)
          (list bit)
          (cons bit (encode-symbol-h symbol branch))))))
       
(define (encode-symbol symbol tree)
  (if (not (has-leaf-sym? tree symbol))
      (error "Error: Symbol not in tree.")
      (encode-symbol-h symbol tree)))
    
(define (leaf-sym? node sym) ; #t iff node is of form '(leaf sym N) for some number N.
  (and (leaf? node) (eq? (symbol-leaf node) sym)))

(define (has-leaf-sym? tree sym) ; #t if tree contains the leaf which has sym
  (cond
    [(null? tree) #f]
    [(leaf? tree) (leaf-sym? tree sym)]
    [(or (has-leaf-sym? (left-branch tree) sym) (has-leaf-sym? (right-branch tree) sym))]
    [else #f]))

;; (encode '(A D A B B C A) sample-tree) ; => '(0 1 1 0 0 1 0 1 0 1 1 1 0)
; *******************************************
; Ex 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge-acc part-tree leaf-set) ; successive merge starting with partially constructed tree, part-tree
  (cond
    [(empty? leaf-set) part-tree]
    [else (successive-merge-acc (make-code-tree (car leaf-set) part-tree) (cdr leaf-set))]))
  
(define (successive-merge leaf-set) ; assume leaf-set has at least 2 leafs since merging is meaningless o.w.
  (let* ([first-2-leaves (take leaf-set 2)]
         [first-2-codetree (make-code-tree (car first-2-leaves) (cadr first-2-leaves))])
         (if (empty? (cddr leaf-set))
             first-2-codetree
             (successive-merge-acc first-2-codetree (cddr leaf-set)))))

; Some vars to experiment in REPL:
(define ps1 '((A 4) (B 2) (C 1) (D 1)))
(define ps1-cd (cddr ps1))
(define ls-cd (make-leaf-set ps1-cd)) ; leaf set of ((C 1) (D 1))
(define ps2 '((B 2) (C 1) (D 1)))
(define st2 '((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4)) ; sample-tree2, which is subtree of st
(define ld1 '(leaf D 1)) ; Larry David is #1! I mean '(leaf D 1), excuse me.
(define lc1 '(leaf C 1))
(define lb2 '(leaf B 2))
(define tdc (make-code-tree ld1 lc1))
(define tbdc (make-code-tree lb2 tdc))

;; (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))) ; => '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)
; *******************************************
; Ex 2.70

(define (pair< p1 p2) (< (cadr p1) (cadr p2)))
(define lyrics (sort '((A 2) (NA	16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)) pair<))
(define lyrics-huff-tree (generate-huffman-tree lyrics))
(define SONG
  '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
; SONG has 36 symbols = 3*36=108 bits if using naive encoding
(define SONG-encoded (encode SONG lyrics-huff-tree))
;; (length SONG-encoded) ; => 87
; so encoding uses 87/108 = 81% as many bits as naive encoding for a 19% reduction - not too shabby!

;;(decode SONG-encoded lyrics-huff-tree) ; => '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM)
; decode returned original (and deeply meaningful) lyrics
; *******************************************
; Misc huffman stuff

(define (remove-all x xs)
  (if (not (memq x xs))
      xs
      (remove-all x (remove-once x xs))))
       
(define (freq sym recado)
  (let ([memq-sym (memq sym recado)])
    (if (not memq-sym)
        0
        (+ 1 (freq sym (cdr memq-sym))))))

(define (make-freq-pair sym recado) ; => (sym K) where K is frequency of sym in recado (message in spanish)
  (list sym (freq sym recado)))
  
(define (make-freq-pairs recado) 
  (if (empty? recado)
      '()
      (let* ([1st-sym (car recado)] [recado-1st-sym-removed (remove-all 1st-sym recado)])
        (let ([unsorted (cons (make-freq-pair 1st-sym recado) (make-freq-pairs recado-1st-sym-removed))])
          (sort unsorted pair<)))))

(define (generate-huff-tree-from-message recado)
  (generate-huffman-tree (make-freq-pairs recado)))
(define generate-huffman-tree-from-message generate-huff-tree-from-message)

(define (encode-message recado)
  (encode recado (generate-huff-tree-from-message recado)))

(define (encode-message-as-string recado) (apply string-append (map number->string (encode-message recado))))

(define (bitcount-naive-encoding recado) ; k total symbols to make recado formed over alphabet of n symbols
  (let* ([k (length recado)] ; k total symbols in recado
        [freq-pairs (make-freq-pairs recado)]
        [n (length freq-pairs)]
        [bits-per-symbol (lg- n)]) ; need ceil(lg(n)) bits to encode each of the n symbols as bits
        (* k bits-per-symbol)))

(define (bitcount-huffman-encoding recado)
  (string-length (encode-message-as-string recado)))

(define (compression-ratio-huffman-encoding recado)
  (let* ([bitcounts (bitcounts-naive-huffman recado)]
         [naive (car bitcounts)]
         [huff (cadr bitcounts)])
    (exact->inexact (/ naive huff))))

; *******************************************
; 2.71
; Let's call such a recado a base2 recado.
(define (make-base2-recado n) ; generate a simple base2 with n symbols: freqs = (1 2 ... 2^(n-1))
  (let* ([freqs (powers 2 (-- n))]
         [symbols (take alphabet n)]
         [freq-pairs (zip symbols freqs)])
    (apply append (map (λ (fp) (repeat (car fp) (cadr fp))) freq-pairs))))

(define r3 (make-base2-recado 3))
(define r3-ht (generate-huffman-tree-from-message r3))

(define r10 (make-base2-recado 10))
(define (bitcounts-naive-huffman recado)
  (list (bitcount-naive-encoding recado) (bitcount-huffman-encoding recado)))

(define (compression-ratios-2-to-N N message-making-proc)
  (map (λ (k) (compression-ratio-huffman-encoding (message-making-proc k))) (range 2 (++ N))))

(define (make-boring-recado n) ; 'a repeated n times
  (repeat 'a n))

(define (make-inclusivity-win-recado n) ; n <= 26, each letter appears exactly once
  (take alphabet n))

;; (compression-ratios-2-to-N 10 make-base2-recado)
; => '(1.0 1.4 1.2 1.6607142857142858 1.588235294117647 1.548780487804878 1.526946107784431 2.0197628458498023 2.0108108108108107)
; It appears to be converging to a 2:1 ratio. 

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