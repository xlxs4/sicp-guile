;;; Exercise 1.36
;; Modify fixed-point so that it prints the sequence
;; of approximations it generates, using the newline
;; and display primitives shown in Exercise 1.22.
;; Then find a solution to x^x = 1000 by finding a
;; fixed point of x ↦ log(1000)/log(x).
;; (Use Scheme's primitive log procedure, which
;; computes natural logarithms.) Compare the number of
;; steps this takes with and without average damping.
;; (Note that you cannot start fixed-point with a guess
;; of 1, as this would causes division by log(1) = 0)

(define (average a b) (/ (+ a b) 2))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess n)
    (define (my-display x)
      (display n) (display ": ")
      (display x) (newline))
    (let ((next (f guess)))
      (my-display next)
      (if (close-enough? guess next)
          next
          (try next (+ n 1)))))
  (try first-guess 1))

(fixed-point (lambda (x)
               (/ (log 1000) (log x))) 3.0) ; 32 iterations
(fixed-point (lambda (x)
               (average x (/ (log 1000) (log x)))) 3.0) ; 8 iterations

;; Take a look at the Banach fixed-point theorem for more ;)
