;;; Exercise 2.81
;; Louis Reasoner has noticed that apply-generic may try to coerce
;; the arguments to each other's type even if they already have
;; the same type. Therefore, he reasons, we need to put procedures
;; in the coercion table to coerce arguments of each type to their
;; own type. For example, in addition to the scheme-number->complex
;; coercion shown above, he would do:

;; (define (scheme-number->scheme-number n) n)
;; (define (complex->complex z) z)

;; (put-coercion 'scheme-number 'scheme-number
;;               scheme-number->scheme-number)

;; (put-coercion 'complex 'complex
;;               complex->complex)

;; 1. With Louis's coercion procedures installed, what happens
;; if apply-generic is called with two arguments of type scheme-number
;; or two arguments of type complex for an operation that is not
;; found in the table for those types? For example, assume that
;; we've defined a generic exponentation operation:

;;(define (exp x y)
;;  (apply-generic 'exp x y))

;; and have put a procedure for exponentiation in the Scheme-number
;; package but not in any other package:

;; following added to Scheme-number package
;; (put 'exp
;;      '(scheme-number scheme-number)
;;      (lambda (x y)
;;        (tag (expt x y))))
;;        ; using primitive expt

;; What happens if we call exp with two complex numbers as arguments?

;; 2. Is Louis correct that something had to be done about coercion
;; with arguments of the same type, or does apply-generic work
;; correctly as is?

;; 3. Modify apply-generic so that it doesn't try coercion if the two
;; arguments have the same type.

;;; Answer
;; 1. The operation is not found in the table for these types, so we
;; enter this part of apply-generic:

;; (cond (t1->t2
;;        (apply-generic
;;         op (t1->t2 a1) a2))
;;       ...)

;; So we're stuck in a recursive loop, where apply-generic calls apply-generic
;; over and over and over again...

;; 2. Not that much of a problem, apply-generic will error as it should.

;; 3.
(define (apply-generic op . args)
  (define (no-method-err op type-tags)
    (error "No method for these types"
           (list op type-tags)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags)))
                (if (not (eq? type1 type2))
                    (let ((a1 (car args))
                          (a2 (cadr args)))
                      (let ((t1->t2
                             (get-coercion type1
                                           type2))
                            (t2->t1
                             (get-coercion type2
                                           type1)))
                        (cond (t1->t2
                               (apply-generic
                                op (t1->t2 a1) a2))
                              (t2->t1
                               (apply-generic
                                op a1 (t2->t1 a2)))
                              (else
                               (no-method-err op type-tags)))))
                    (no-method-err op type-tags)))
              (no-method-err op type-tags))
          (no-method-err op type-tags)))))
