;;; Exercise 2.17
;; Define a procedure last-pair that returns the list
;; the contains only the last element of a given
;; (nonempty) list:

;; (last-pair (list 23 72 149 34))
;; (34)

(define (last-pair xs)
  (if (null? xs)
      '()
      (let ((tail (cdr xs)))
        (if (null? tail)
            (car xs)
            (last-pair tail)))))
