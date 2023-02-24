;;; Exercise 1.35
;; Show that the golden ratio φ (1.2.2) is a
;; fixed point of the transformation
;; x ↦ 1 + 1/x, and use this fact to compute φ
;; by means of the fixed-point procedure.

;;; Answer:
;; x ↦ 1 + 1/x means that
;; x = 1 + 1/x ⇒ x^2 = x + 1 ⇒ x^2 - x - 1 = 0
;;
;; The discriminant is 5 > 0, so we get
;;
;;      -b ± sqrt(b^2 - 4ac)
;; x = ----------------------
;;               2a
;;
;;      1 ± sqrt(5)
;; x = -------------
;;           2
;;
;; So, one solution is indeed
;;
;;      1 + sqrt(5)
;; x = ------------- = φ
;;           2

(define (average a b) (/ (+ a b) 2))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; Therefore, the solution is:

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

;; And, with average damping:

(fixed-point (lambda (x) (average x (+ 1 (/ 1 x)))) 1.0)
