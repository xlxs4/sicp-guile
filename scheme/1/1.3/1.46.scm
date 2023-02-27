;;; Exercise 1.46
;; Several of the numerical methods described in this
;; chapter are instances of an extremely general
;; computational strategy known as *iterative improvement*.
;; Iterative improvement says that, to compute something,
;; we start with an initial guess for the answer, test
;; if the guess is good enough, and otherwise improve the
;; guess and continue the process using the improved guess
;; as the new guess. Write a procedure iterative-improve
;; that takes two procedures as arguments: a method for
;; telling whether a guess is good enough and a method
;; for improving a guess. iterative-improve should return
;; as its value a procedure that takes a guess as argument
;; and keeps improving the guess until it is good enough.
;; Rewrite the sqrt procedure of 1.1.7 and the fixed-point
;; procedure of 1.3.3 in terms of iterative-improve.

(define (square x) (* x x))
(define (average a b) (/ (+ a b) 2))

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define (my-sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

;; To me this is why this exercise was made.
;; You come to notice that good-enough? for sqrt
;; takes one argument but good-enough? for fixed-point
;; takes in two arguments. How do we work around this?
;; Well, since our iterative-improve expects a
;; good-enough? that takes one argument, we pass it a
;; procedure that takes in one argument and calls
;; the actual good-enough? with x and (f x) passed to it,
;; just like the

;; (let ((next (f guess)))
;;   (if (close-enough? guess next) [...]

;; in the original. This is a powerful trick, don't miss it.

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve
    (lambda (x) (close-enough? x (f x)))
    f)
   first-guess))
