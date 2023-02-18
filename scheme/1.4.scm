;;; Exercise 1.4
;; Observe that our model of evaluation allows for combinations
;; whose operators are compound expressions. Use this observation
;; to describe the behavior of the following procedure:

;; (define (a-plus-abs-b a b)
;;   ((if (> b 0) + -) a b))

;;; Answer:
;; The if statement returns either - or +,
;; which is then applied to the operands:
;; If b is greater than 0, then a + b.
;; Else, a - b.

