;;; Exercise 1.5
;; Ben Bitdiddle has invented a test to determine whether
;; the interpreter he is faced with is using
;; applicative-order evaluation or normal-order evaluation.
;; He defines the following two procedures:

;; (define (p) (p))

;; (define (test x y)
;;   (if (= x 0)
;;       0
;;       y))

;; Then he evaluates the expression

;; (test 0 (p))

;; What behavior will Ben observe with the interpreter
;; that uses applicative-order evaluation? What behavior
;; will he observe with an interpreter that uses normal-order evaluation?
;; Explain your answer. (Assume that the evaluation rule for the
;; special form if is the same whether the interpreter is using
;; normal or applicative order: The predicate expression is evaluated first,
;; and the result determines whether to evaluate
;; the consequent or the alternative expression.)

;;; Answer:
;; If the interpreter uses applicative-order evaluation,
;; the argument subexpressions are evaluated first.
;; These are 0 and (p). 0 evaluates to the value 0.
;; (p) is a call to the procedure p. Following on with the evaluation order,
;; we can think of p being "replaced" with the procedure body.
;; The procedure body is, again, (p), which means evaluating the procedure
;; requires evaluating the procedure, and we're stuck
;; in what can be thought of as an "evaluation loop".
;; While the procedure is an argument that we know will
;; not be used when evaluating the special form if,
;; it still is responsible for non-terminating evaluation.

;; This wouldn't happen with normal-order evaluation.
;; We can think that the call is replaced with the body (the if form)
;; and then that the formal arguments x and y are
;; replaced with 0 and (p). We end up at (if (= 0 0) 0 (p)).
;; The predicate expression is evaluated first, which means the
;; consequent, 0, is evaluated next. We get the value of 0
;; and evaluation stops there.

;; This isn't the whole story — there's still environments and renaming
;; to think about, but that's a good enough explanation for now.

