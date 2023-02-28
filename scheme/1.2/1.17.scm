;;; Exercise 1.17
;; The exponentiation algorithms in this section are based on
;; performing exponentiation by means of repeated multiplication.
;; In a similar way, one can perform integer multiplication
;; by means of repeated addition. The following multiplication procedure
;; (in which it is assumed that our language can only add, not multiply)
;; is analogous to the expt procedure:

;; (define (* a b)
;;   (if (= b 0)
;;       0
;;       (+ a (* a (- b 1)))))

;; This algorithm takes a number of steps that is linear in b.
;; Now suppose we include, together with addition, operations double,
;; which doubles an integer, and halve, which divides an (even) integer by 2.
;; Using these, design a multiplication procedure analogous to fast-expt
;; that uses a logarithmic number of steps.

;;; Answer:
;; "[...] the technique of defining an invariant quantity that
;; remains unchainged from state to state is a powerful way
;; to think about the design of iterative algorithms."

;; When b is even, we have the following invariant:
;; ab = a · (2 · (b/2)) = 2a · b/2
;; a' = 2a
;; b' = b/2

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast-mul a b)
  (cond ((= b 0)
         0)
        ((even? b)
         (fast-mul (double a) (halve b)))
        (else
         (+ a (fast-mul a (- b 1))))))

