;;; Exercise 1.15
;; The sine of an angle (specified in radians) can be computed by
;; making use of the approximation sin x == approx(x)
;; if x is sufficiently small, and the trigonometric identity

;; sin x = 3sin(x / 3) - 4sin(x / 3)^3

;; to reduce the size of the argument of sin.
;; (For purposes of this exercise an angle is considered "sufficiently small"
;; if its magnitude is not greater than 0.1 radians.)
;; These ideas are incorporated in the following procedures:

;; (define (cube x) (* x x x))
;; (define (p x) (- (* 3 x) (* 4 (cube x))))
;; (define (sine angle)
;;   (if (not (> (abs angle) 0.1))
;;       angle
;;       (p (sine (/ angle 3.0)))))

;; 1. How many times is the procedure p applied when (sine 12.15) is evaluated?
;; 2. What is the order of growth in space and number of steps (as a function of α)
;; used by the process generated by the sine procedure when (sine a) is evaluated?

;;; Answer:
;; scheme@(guile-user)> ,trace (sine 12.15)
;; trace: |  (sine 12.15)
;; trace: |  |  (sine 4.05)
;; trace: |  |  |  (sine 1.3499999999999999)
;; trace: |  |  |  |  (sine 0.44999999999999996)
;; trace: |  |  |  |  |  (sine 0.15)
;; trace: |  |  |  |  |  |  (sine 0.049999999999999996)
;; trace: |  |  |  |  |  (p 0.049999999999999996)
;; trace: |  |  |  |  (p 0.1495)
;; trace: |  |  |  (p 0.4351345505)
;; trace: |  |  (p 0.9758465331678772)
;; trace: |  (p -0.7895631144708228)

;; The procedure p will be applied 5 times.
;; For every iteration, α is divided by 3.
;; The stopping criterion can be expressed as:
;; α / 3^n < 0.1
;; α / 0.1 < 3^n
;; log(α / 0.1) < log(3^n)
;; log(α) - log(0.1) < n log(3)
;; (log(α) - log(0.1)) / log(3) < n

;; That is to say, the procedure needs to be applied about
;; log(10α) / log(3) times when computing (sine a).

;; The order of growth of space and of number of steps is Θ(log α).

