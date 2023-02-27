;;; Exercise 1.45
;; We saw in 1.3.3 that attempting to compute
;; square roots by naively finding a fixed point
;; of y ↦ x/y does not converge, and that this
;; can be fixed by average damping. The same
;; method works for finding cube roots as fixed
;; points of the average-damped y ↦ x/y^2.
;; Unfortunately, the process does not work for fourth
;; roots — a single average damp is not enough to make
;; a fixed-point search for y ↦ x/y^3 converge.
;; On the other hand, if we average damp twice (i.e.,
;; use the average damp of the average damp of y ↦ x/y^3)
;; the fixed-point search does converge. Do some
;; experiments to determine how many average damps are
;; required to compute nth roots as a fixed-point search
;; based upon repeated average damping of y ↦ x/y^(n-1).
;; Use this to implement a simple procedure for computing
;; nth roots using fixed-point, average-damp, and the
;; repeated procedure of Exercise 1.43. Assume that any
;; arithmetic operations you need are available as primitives.

(define (average a b) (/ (+ a b) 2))
(define (double x) (* 2 x))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; Recall that we did

;; (define (sqrt x)
;;   (fixed-point
;;    (average-damp
;;     (lambda (y) (/ x y)))
;;    1.0))

;; Testing by hand can become tedious very quick.
;; This is a chapter about higher-order procedures
;; after all...

(define (nth-root-damped x n damp)
  (fixed-point
   ((repeated average-damp damp)
    (lambda (y)
      (/ x (expt y (- n 1)))))
   1.0))

;; scheme@(guile-user)> (nth-root-damped 2 2 1)
;; 1.4142135623746899
;; scheme@(guile-user)> (nth-root-damped 2 3 1)
;; 1.259923236422975
;; scheme@(guile-user)> (nth-root-damped 2 4 2)
;; 1.189207115002721
;; scheme@(guile-user)> (nth-root-damped 2 5 2)
;; 1.1486967244204176
;; scheme@(guile-user)> (nth-root-damped 2 6 2)
;; 1.1224648393618204
;; scheme@(guile-user)> (nth-root-damped 2 7 2)
;; 1.1040857488809648
;; scheme@(guile-user)> (nth-root-damped 2 8 3)
;; 1.090507732665258
;; scheme@(guile-user)> (nth-root-damped 2 9 3)
;; 1.0800601441048037
;; scheme@(guile-user)> (nth-root-damped 2 10 3)
;; 1.0717742428174573
;; scheme@(guile-user)> (nth-root-damped 2 11 3)
;; 1.065039586617723
;; scheme@(guile-user)> (nth-root-damped 2 12 3)
;; 1.059461368044972
;; scheme@(guile-user)> (nth-root-damped 2 13 3)
;; 1.0547695373814245
;; scheme@(guile-user)> (nth-root-damped 2 14 3)
;; 1.050752520212518
;; scheme@(guile-user)> (nth-root-damped 2 15 3)
;; 1.012916009423302
;; scheme@(guile-user)> (nth-root-damped 2 16 4)
;; 1.0442737824274142

;; etc. We can see that damp needs to increase when
;; n reaches a new power of 2. In other words, this means
;; that the damping y ↦ x/y^(n-1) must be repeated
;; log_2(n) times to compute the nth root of x.
;; Therefore:

(define (nth-root x n)
  (define (log2 x)
    (/ (log x) (log 2))) ; log_k(n) = ln(n) / ln(k)
  (fixed-point
   ((repeated average-damp (floor (log2 n)))
    (lambda (y) (/ x (expt y (- n 1)))))
   1.0))
