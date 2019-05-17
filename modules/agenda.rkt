(module agenda racket
  ;(require "queue.rkt")
  ;(require r5rs) ; for set-car! and related
  (require "queue.rkt" r5rs)
  
  (provide the-agenda ; just one agenda right now, TODO: generalize.
           make-agenda  empty-agenda?
           first-agenda-item
           remove-first-agenda-item! add-to-agenda!
           current-time
           propagate
           after-delay
           ;make-time-segment segment-time segment-queue current-time
          ; set-current-time! segments set-segments! first-segment rest-segments
           )

  (define the-agenda (list 0)) 
  
  (define (make-time-segment time queue)
    (cons time queue))
  (define (segment-time s) (car s))
  (define (segment-queue s) (cdr s))
  (define (make-agenda) (list 0)) ;returns a new empty agenda
  
  (define (current-time agenda) (car agenda))

  (define (set-current-time! agenda time)
    (set-car! agenda time))
  (define (segments agenda) (cdr agenda))
  (define (set-segments! agenda segments)
    (set-cdr! agenda segments))

  (define (first-segment agenda) (car (segments agenda)))
  (define (rest-segments agenda) (cdr (segments agenda)))

  (define (empty-agenda? agenda) ; is true if the specified agenda is empty
    (null? (segments agenda)))

  (define (add-to-agenda! time action agenda)
    ;modifies the agenda by adding the given action procedure to be run at the specified time.
    (define (belongs-before? segments) ;return true if our appointed time **isn't** in segements
      (or (null? segments)
          (< time (segment-time (car segments)))))

    (define (make-new-time-segment time action) ;create a new segment for our time and action
      (let ((q (make-queue)))
        (insert-queue! q action)
        (make-time-segment time q)))

    (define (add-to-segments! segments)
      (if (= (segment-time (car segments)) time)
          (insert-queue! (segment-queue (car segments))
                         action) ;insert action into the queue our time associated
          (let ((rest (cdr segments)))
            (if (belongs-before? rest)
                (set-cdr!
                 segments
                 (cons (make-new-time-segment time action)
                       (cdr segments))) ;insert new segement before the later or just at the end of agenda
                (add-to-segments! rest)))))

    (let ((segments (segments agenda)))
      (if (belongs-before? segments) ;check if the agenda is null or just contains the later segements
          (set-segments!
           agenda
           (cons (make-new-time-segment time action)
                 segments)) ;insert new segement at the head of segements
          (add-to-segments! segments))))
  ; END add-to-agenda!
      
  (define (remove-first-agenda-item! agenda) ;modifies the agenda by removing the first item
    (let ((q (segment-queue (first-segment agenda))))
      (delete-queue! q)
; The following "if" was changed to "unless" since racket doesn't have 2-adic if.
;      (if (empty-queue? q)
;          (set-segments! agenda (rest-segments agenda)))
      (unless (not (empty-queue? q))
        (set-segments! agenda (rest-segments agenda)))
      ))

  (define (first-agenda-item agenda) ;returns the first item on the agenda
    (if (empty-agenda? agenda)
        (error "Agenda is empty: FIRST-AGENDA-ITEM")
        (let ((first-seg (first-segment agenda)))
          (set-current-time! agenda
                             (segment-time first-seg))
          (front-queue (segment-queue first-seg)))))

  (define (after-delay delay action)
    (add-to-agenda! (+ delay (current-time the-agenda))
                    action
                    the-agenda))
                
  (define (propagate)
    (if (empty-agenda? the-agenda)
        'done
        (let ((first-item (first-agenda-item the-agenda)))
          (first-item)
          (remove-first-agenda-item! the-agenda)
          (propagate))))

  ) ; End AGENDA module
