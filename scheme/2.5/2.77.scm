;;; Exercise 2.77
;; Louis Reasoner tries to evaluate the expression (magnitude z)
;; where z is the object shown in Figure 2.24. To his surprise,
;; instead of the answer 5 he gets an error message from apply-generic,
;; saying there is no method for the operation magnitude on the types
;; (complex). He shows this interaction to Alyssa P. Hacker, who says
;; "The problem is that the complex-number selectors were never defined
;; for complex numbers, just for polar and rectangular numbers.
;; All you have to do to make this work is add the following to the
;; complex package:"

;; (put 'real-part '(complex) real-part)
;; (put 'imag-part '(complex) imag-part)
;; (put 'magnitude '(complex) magnitude)
;; (put 'angle '(complex) angle)

;; Describe in detail why this works. As an example, trace through all
;; the procedures called in evaluating the expression (magnitude z)
;; where z is the object shown in Figure 2.24. In particular, how many
;; times is apply-generic invoked? What procedure is dispatched to in
;; each case?

;;; Answer
;; (magnitude z) is defined as such:

;; (define (magnitude z)
;;   (apply-generic 'magnitude z))

;; and this is apply-generic:

;; (define (apply-generic op . args)
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get op type-tags)))
;;       (if proc
;;           (apply proc (map contents args))
;;           (error
;;             "No method for these types:
;;              APPLY-GENERIC"
;;             (list op type-tags))))))

;; type-tag:

;; (define (type-tag datum)
;;   (if (pair? datum)
;;       (car datum)
;;       (error "Bad tagged datum:
;;               TYPE-TAG" datum)))

;; so, type-tags is 'complex
;; but (get 'magnitude 'complex) doesn't resolve to a procedure,
;; since, as Alyssa said, the selectors weren't defined to work
;; with 'complex numbers.
;; After these:

;; (put 'real-part '(complex) real-part)
;; (put 'imag-part '(complex) imag-part)
;; (put 'magnitude '(complex) magnitude)
;; (put 'angle '(complex) angle)

;; then proc is the magnitude procedure.

;; (define (contents datum)
;;   (if (pair? datum)
;;       (cdr datum)
;;       (error "Bad tagged datum:
;;               CONTENTS" datum)))

;; magnitude is applied to ('rectangular . (3 4)).
;; It invokes apply-generic again:
;; (define (magnitude z)
;;   (apply-generic 'magnitude z))

;; and it can be found in the rectangular package:

;; (define (magnitude z)
;;   (sqrt (+ (square (real-part z))
;;            (square (imag-part z)))))

;; So what Alyssa did essentially acts as a pass-through.
