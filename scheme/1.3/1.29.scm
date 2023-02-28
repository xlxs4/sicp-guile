;;; Exercise 1.29
;; Simpson's Rule is a more accurate method of
;; numerical integration than the method illustrated
;; above. Using Simpson's Rule, the integral of a
;; function f between a and b is approximated as

;;  h
;; --- (y0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + ... +
;;  3
;;
;; + 2y_(n-2) + 4y_(n-1) + yn)

;; where h = (b - a)/n, for some even integer n, and
;; y_k = f(a + kh). (Increasing n increases the accuracy
;; of the approximation.) Define a procedure that takes as
;; arguments f, a, b, and n and returns the value of the
;; integral, computed using Simpson's Rule. Use your procedure
;; to integrate cube between 0 and 1 (with n = 100 and n = 1000),
;; and compare the results to those of the integral procedure
;; shown above.

(define (inc x) (+ x 1))

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* h k))))
  (define (term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((odd? k) 4)
             (else 2))
       (yk k)))
  (* (/ h 3)
     (sum term 0 inc n)))

;; scheme@(guile-user)> (integral cube 0 1 0.01)
;; 0.24998750000000042
;; scheme@(guile-user)> (integral cube 0 1 0.001)
;; 0.249999875000001

;; we get exact answers!
;; scheme@(guile-user)> (integral cube 0 1 100)
;; 1/4
;; scheme@(guile-user)> (integral cube 0 1 1000)
;; 1/4
