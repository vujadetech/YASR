; QUEUE *****************************************************************
;#lang racket
; NB: This module uses set-car! which can be provided by either r5rs or rnrs/mutable-pairs-6.
(module queue racket
  (require r5rs) ; for set-car! NB: rnrs/mutable... didn't work
  ;(require rnrs/mutable-pairs-6)
  (provide front-ptr rear-ptr set-front-ptr! set-rear-ptr! empty-queue? make-queue
           front-queue insert-queue! delete-queue!)
           
  (define (front-ptr queue) (car queue))
  (define (rear-ptr queue) (cdr queue))
  (define (set-front-ptr! queue item) (set-car! queue item))
  (define (set-rear-ptr! queue item) (set-cdr! queue item))
  (define (empty-queue? queue) (null? (front-ptr queue)))
  (define (make-queue) (cons '() '()))

  (define (front-queue queue)
    (if (empty-queue? queue)
        (error "FRONT called with an empty queue" queue)
        (car (front-ptr queue))))
    
  (define (insert-queue! queue item)
    (let ((new-pair (cons item '())))
      (cond ((empty-queue? queue)
             (set-front-ptr! queue new-pair)
             (set-rear-ptr! queue new-pair)
             queue)
            (else
             (set-cdr! (rear-ptr queue) new-pair)
             (set-rear-ptr! queue new-pair)
             queue))))
         
  (define (delete-queue! queue)
    (cond ((empty-queue? queue)
           (error "DELETE! called with an empty queue" queue))
          (else
           (set-front-ptr! queue (cdr (front-ptr queue)))
           queue)))
  ) ; end QUEUE module
; end QUEUE *****************************************************************