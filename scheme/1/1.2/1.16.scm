;;; Exercise 1.16
;; Design a procedure that evolves an iterative exponentation process
;; that uses successive squaring and uses a logarithmic number of steps,
;; as does fast-expt. (Hint: Using the observation that
;; (b^(n/2))^2 = (b^2)^(n/2), keep, along with the exponent n
;; and the base b, an additional state variable a,
;; and define the transformation in such a way that
;; the product ab^n is unchanged from state to state. At the beginning
;; of the process a is taken to be 1, and the answer is given by the value
;; of a at the end of the process. In general, the technique of defining
;; an invariant quantity that remains unchainged from state to state
;; is a powerful way to think about the design of iterative algorithms.)

;;; Answer:
;; We want to have ab^n unchanged from state to state.
;; We need to think about two cases:
;; When n is even, ab^n can be rewritten as:
;; ab^n = a(b^(n/2)) = a(b^2)^(n/2)
;; This can be written as a'(b')^n',
;; where a' = a, b' = b^2, n' = n/2

;; Similarly, when n is odd, ab^n can be rewritten as:
;; ab^n = abb^(n - 1) = a'(b')^n'
;; where a' = ab, b' = b, n' = n - 1
;; That's all we need.

;; acc stands for accumulator, which is basically what
;; the additional state variable we're introducing is.

(define (square x) (* x x))

(define (fast-expt b n)
  (define (iter b n acc)
    (cond ((= n 0)
           acc)
          ((even? n)
           (iter (square b) (/ n 2) acc))
          (else
           (iter b (- n 1) (* acc b)))))
  (iter b n 1))

;; This is Θ(1) for space and Θ(log n) for number of steps.

