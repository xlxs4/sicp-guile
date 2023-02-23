;;; Exercise 1.3
;; Define a procedure that takes three numbers as arguments
;; and returns the sum of the squares of the two larger numbers.

(define (square x) (* x x))

(define (largest-squares x y z)
  (define (sum-squares a b) (+ (square a) (square b)))

  (cond ((and (<= x y) (<= x z)) (sum-squares y z))
        ((and (<= y x) (<= y z)) (sum-squares x z))
        (else (sum-squares x y))))

