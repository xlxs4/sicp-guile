;;; Exercise 2.5
;; Show that we can represent pairs of nonnegative
;; integers using only numbers and arithmetic operations
;; if we represent the pair a and b as the integer that
;; is the product 2^a ✕ 3^b. Give the corresponding
;; definitions of the procedures cons, car, and cdr.

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (largest-power-divisor x z)
  (if (= (remainder z x) 0)
      (+ (largest-power-divisor x (/ z x)))
      0))

(define (car z)
  (largest-power-divisor 2 z))

(define (cdr z)
  (largest-power-divisor 3 z))

;; Some trivia: This is related to 2-adic valuation,
;; and describes 3-smooth nonnegative integers.
;; There's also a data structure called a 2-3 tree,
;; if you want to look into that.
