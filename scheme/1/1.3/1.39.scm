;;; Exercise 1.39
;; A continued fraction representation of the tangent
;; function was published in 1770 by the German
;; mathematician J.H. Lambert:
;;
;;                    x
;; tan x = -----------------------
;;                     x^2
;;           1 - ----------------
;;                        x^2
;;                 3 - ---------
;;                      5 - ...
;;
;; where x is in radians. Define a procedure
;; (tan-cf x k) that computes an approximation to the
;; tangent function based on Lambert's formula.
;; k specifies the numebr of terms to compute, as in
;; Exercise 1.37.

(define (square x) (* x x))

(define (cont-frac n d k)
  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (- i 1) (/ (n i) (+ (d i) acc)))))
  (iter k 0))

(define (tan-cf x k)
  (cont-frac
   (lambda (i) (if (= i 1) x (* x x -1)))
   (lambda (i) (- (* i 2) 1))
   k))
