;;; Exercise 1.18
;; Using the results of Exercise 1.16 and Exercise 1.17, devise a procedure
;; that generates an iterative process for multiplying two integers
;; in terms of adding, doubling, and halving and uses a logarithmic number of steps.

;;; Answer:
;; We need to introduce another variable, a state variable.
;; Again, there are two cases for which we must come up with an invariant:
;; When b is even, we have (from above):
;; a' = 2a
;; b' = b/2
;; acc' = acc

;; When b is odd:
;; ab + n = a · (1 + (b - 1)) + n = a · (b - 1) + (n + a)
;; a' = a
;; b' = (b - 1)
;; n' = n + a

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast-mul a b)
  (define (iter a b acc)
    (cond ((= b 0)
           acc)
          ((even? b)
           (iter (double a) (halve b) acc))
          (else
           (iter a (- b 1) (+ acc a)))))
  (iter a b 0))

