;;; Exercise 1.20
;; The process that a procedure generates is of course
;; dependent on the rules used by the interpreter.
;; As an example, consider the iterative gcd procedure given above.
;; Suppose we were to interpret this procedure using normal-order evaluation,
;; as discussed in 1.1.5. (The normal-order-evaluation rule for
;; if is described in Exercise 1.5.) Using the substitution method
;; (for normal order), illustrate the process generated in evaluating
;; (gcd 206 40) and indicate the remainder operations that are actually performed.
;; How many remainder operations are actually performed
;; in the normal-order evaluation of (gcd 206 40)?
;; In the applicative-order evaluation?

;;; Answer:
;; Using normal-order evaluation, remainder is called 18 times.
;; 14 of which are to evaluate the condition, and an additional
;; 4 during the final reduction evaluation phase.
;; Using applicative-order evaluation, remainder is called 4 times.

