;;; Exercise 2.80
;; Define a generic predicate =zero? that tests if its argument is zero,
;; and install it in the generic arithmetic package. This operation
;; should work for ordinary numbers, rational numbers, and complex numbers.

;;; Answer
;; In install-scheme-number-package

;; (put '=zero? 'scheme-number
;;      (lambda (x) (zero? x)))

;; In install-rational package

;; (define (=zero?-rat x)
;;   (zero? (numer x)))

;; In install-complex package

;; (define (=zero?-complex x)
;;   (and (zero? (real-part x))
;;        (zero? (imag-part x))))
