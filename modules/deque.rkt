(module deque racket
 ; (require r5rs)
  (require "queue.rkt")
  (provide make-deque)

  (define make-deque make-queue))