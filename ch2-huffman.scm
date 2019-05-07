#lang racket

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

; The following procedure takes a list of symbol-frequency pairs such as ((A 4) (B 2) (C 1) (D 1)) and constructs an initial ordered set of leaves, ready to be merged according to the Huffman algorithm:

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

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; (decode sample-message sample-tree) ; => '(A D A B B C A)
; *******************************************
; Ex 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol-h symbol tree) ; symbol is assumed to be in tree for the helper function
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
; *******************************************
; *******************************************
; *******************************************
; *******************************************
; *******************************************
; *******************************************
; *******************************************
; *******************************************
; *******************************************