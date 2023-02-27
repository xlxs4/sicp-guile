;;; Exercise 1.43
;; If f is a numerical function and n is a positive
;; integer, then we can form the nth repeated
;; application of f, which is defined to be the
;; function whose value at x is f(f(...(f(x))...)).
;; For example, if f is the function x ↦ x + 1,
;; then the nth repeated application of f is the
;; function x ↦ x + n. If f is the operation of
;; squaring a number, then the nth repeated application
;; of f is the function that raises its argument to the
;; 2^n-th power. Write a procedure that takes as inputs
;; a procedure that computes f and a positive integer n
;; and returns the procedure that computes the nth
;; repeated application of f. Your procedure should be
;; able to be used as follows:

;; ((repeated square 2) 5)
;; 625

(define (square x) (* x x))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;; scheme@(guile-user)> ((repeated square 2) 5)
;; 625

;; Bonus: Note the successive application of compose.
;; This is like sum! It should hint at a use of accumulate:

(define (accumulate
         combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (repeated f n)
  (accumulate compose identity (lambda (i) f) 1 inc n))

;; We can also do iterative, with succssive halving/doubling
;; for Θ(log(n))

(define (repeated f n)
  (define (iter n acc)
    (cond ((= 1 n) acc)
          ((even? n) (double (iter (/ n 2) acc)))
          (else (compose f (iter (- n 1) (compose f acc))))))
  (iter n f))
