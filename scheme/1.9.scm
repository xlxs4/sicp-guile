;;; Exercise 1.9
;; Each of the following two procedures defines a method
;; for adding two positive integers in terms of the procedures
;; inc, which increments its argument by 1, and dec,
;; which decrements its argument by 1.

;; (define (+ a b)
;;   (if (= a 0)
;;       b
;;       (inc (+ (dec a) b))))

;; (define (+ a b)
;;   (if (= a 0)
;;       b
;;       (+ (dec a) (inc b))))

;; Using the substitution model, illustrate the process generated
;; by each procedure in evaluating (+ 4 5). Are these processes
;; iterative or recursive?

;;; Answer:

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

;; Let's take a look at the first version <trace simplified>:

;; scheme@(guile-user)> ,trace (+ 4 5)
;; trace: |  (+ 4 5)
;; trace: |  |  (+ 3 5)
;; trace: |  |  |  (+ 2 5)
;; trace: |  |  |  |  (+ 1 5)
;; trace: |  |  |  |  |  (+ 0 5)
;; trace: |  |  |  |  |  5
;; trace: |  |  |  |  6
;; trace: |  |  |  7
;; trace: |  |  8
;; trace: |  9

;; And the second version <trace simplified>:

;; scheme@(guile-user)> ,trace (+ 4 5)
;; trace: |  (+ 4 5)
;; trace: |  (+ 3 6)
;; trace: |  (+ 2 7)
;; trace: |  (+ 1 8)
;; trace: |  (+ 0 9)
;; trace: |  9

;; Both versions are a definition of a recursive procedure.
;; However, the first version generates a recursive process,
;; while the second version generates an iterative process.
;; Take a look at 1.2.1 "Linear Recursion and Iteration".

