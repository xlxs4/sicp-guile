;;; Exercise 1.34
;; Suppose we define the procedure

;; (define (f g) (g 2))

;; Then we have

;; (f square)
;; 4

;; (f (lambda (z) (* z (+ z 1))))
;; 6

;; What happens if we (perversely) ask the interpreter
;; to evaluate the combination (f f)? Explain.

;;; Answer:
;; (f f) yields
;; (f 2) which, in turn, yields
;; (2 2)

;; Guile Scheme errors "Wrong type to apply: 2",
;; MIT/GNU Scheme errors "The object 2 is not applicable," etc.
