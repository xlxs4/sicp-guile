;;; Exercise 2.1
;; Define a better version of make-rat that handles
;; both positive and negative arguments. make-rat
;; should normalize the sign so that if the rational
;; number is positive, both the numerator and denominator
;; are positive, and if the rational number is negative,
;; only the numerator is negative.

(define (make-rat n d)
  (define (neg x) (* x -1))
  (let ((g (gcd n d)))
    (if (negative? d)
        (cons (/ (neg n) g)
              (/ (neg d) g))
        (cons (/ n g)
              (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))
