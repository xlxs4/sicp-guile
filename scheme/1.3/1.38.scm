;;; Exercise 1.38
;; In 1737, the Swiss mathematician Leonhard Euler
;; published a memoir De Fractionibus Continuis,
;; which included a continued fraction expansion for
;; e - 2, where e is the base of the natural logarithms.
;; In this fraction, the Ni are all 1, and the Di are
;; successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ....
;; Write a program that uses your cont-frac procedure
;; from Exercise 1.37 to approximate e, based on
;; Euler's expansion.

(define (cont-frac n d k)
  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (- i 1) (/ (n i) (+ (d i) acc)))))
  (iter k 0))

(define (approx-two-minus-e k)
  (define (di i)
    (if (= (modulo i 3) 2)
        (* 2 (/ (+ i 1) 3))
        1))
  (exact->inexact ; to get a float
   (cont-frac (lambda (i) 1)
              di
              k)))

;; e - 2 ≅ 0.7182818284590452
;; scheme@(guile-user)> (approx-two-minus-e 19)
;; 0.7182818284590459
;; scheme@(guile-user)> (approx-two-minues-e 20)
;; 0.7182818284590452
