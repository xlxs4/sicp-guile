;;; Exercise 2.79
;; Define a generic equality predicate equ? that tests the equality
;; of two numbers, and install it in the generic arithmetic package.
;; This operation should work for ordinary numbers, rational numbers,
;; and complex numbers.

;;; Answer
;; In install-scheme-number-package

;; (put 'equ? '(scheme-number scheme-number)
;;      (lambda (x y) (tag (= x y))))


;; In install-rational-package

;; (define (equ?-rat x y)
;;   (and (= (numer x) (numer y))
;;        (= (denom x) (denom y))))

;; (put 'equ? '(rational rational)
;;      (lambda (x y) (tag (equ?-rat x y))))

;; In install-complex-package

;; (define (equ?-complex x y)
;;   (and (= (real-part x) (real-part y))
;;        (= (imag-part x) (imag-part y))))

;; (put 'equ? '(complex complex)
;;      (lambda (x y) (tag (equ?-complex x y))))
