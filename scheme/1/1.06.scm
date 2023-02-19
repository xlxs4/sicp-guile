;;; Exercise 1.6
;; Alyssa P. Hacker doesn't see why if needs to be
;; provided as a special form. "Why can't I just define it
;; as an ordinary procedure in terms of cond?" she asks.
;; Alyssa's friend Eva Lu Ator claims that this can indeed be done,
;; and she defines a new version of if:

;; (define (new-if predicate
;;                 then-clause
;;                 else-clause)
;;   (cond (predicate then-clause)
;;         (else else-clause)))

;; Eva demonstrates the program for Alyssa:

;; (new-if (= 2 3) 0 5)
;; 5

;; (new-if (= 1 1) 0 5)
;; 0

;; Delighted, Alyssa uses new-if to rewrite the square-root program:

;; (define (sqrt-iter guess x)
;;   (new-if (good-enough? guess x)
;;           guess
;;           (sqrt-iter (improve guess x) x)))

;; What happens when Alyssa attempts to use this to compute
;; square roots? Explain.

;;; Answer:
;; Scheme is an applicative-order language. As we saw in Exercise 1.5,
;; if new-if is an ordinary procedure instead of a special form,
;; that means that the predicate, the consequent, and the alternative
;; will all be evaluated before evaluating the procedure itself.
;; We can see that sqrt-iter is recursively defined —
;; calling new-if would mean we need to evaluate sqrt-iter while
;; we're still evaluating sqrt-iter, which results in an evaluation loop.

