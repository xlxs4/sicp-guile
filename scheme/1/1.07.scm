;;; Exercise 1.7
;; The good-enough? test used in computing square roots will not
;; be very effective for finding the square roots of very small numbers.
;; Also, in real computers, arithmetic operations are almost always
;; performed with limited precision. This makes our test inadequate
;; for very large numbers. Explain these statements, with examples
;; showing how the test fails for small and large numbers.
;; An alternative strategy for implementing good-enough?
;; is to watch how guess changes from one iteration to the next
;; and to stop when the change is a very small fraction of the guess.
;; Design a square-root procedure that uses this kind of end test.
;; Does this work better for small and large numbers?

;;; Answer:
;; Let's look into small numbers first.

;; scheme@(guile-user)> (my-sqrt 1.0e-8)
;; 0.03125010656242753

;; (define (square x) (* x x))
;; scheme@(guile-user)> (square (my-sqrt 1.0e-8))
;; $9 = 9.76569160163076e-4

;; scheme@(guile-user)> (use-modules (system vm trace))
;; scheme@(guile-user)> ,trace (my-sqrt 1.0e-8)
;; trace: |  (my-sqrt 1.0e-8)
;; trace: |  |  (square 1.0)
;; trace: |  |  1.0
;; trace: |  |  (average 1.0 1.0e-8)
;; trace: |  |  0.500000005
;; trace: |  |  (square 0.500000005)
;; trace: |  |  0.25000000499999997
;; trace: |  |  (average 0.500000005 1.9999999800000004e-8)
;; trace: |  |  0.25000001249999987
;; trace: |  |  (square 0.25000001249999987)
;; trace: |  |  0.06250000625000009
;; trace: |  |  (average 0.25000001249999987 3.999999800000012e-8)
;; trace: |  |  0.12500002624999892
;; trace: |  |  (square 0.12500002624999892)
;; trace: |  |  0.01562500656250042
;; trace: |  |  (average 0.12500002624999892 7.999998320000422e-8)
;; trace: |  |  0.06250005312499106
;; trace: |  |  (square 0.06250005312499106)
;; trace: |  |  0.003906256640626705
;; trace: |  |  (average 0.06250005312499106 1.599998640001385e-7)
;; trace: |  |  0.03125010656242753
;; trace: |  |  (square 0.03125010656242753)
;; trace: |  |  9.76569160163076e-4
;; trace: |  0.03125010656242753

;; Since good-enough? uses a tolerance of 0.001 it follows that
;; our sqrt procedure cannot provide a result
;; smaller than about the tolerance, which renders the procedure
;; ineffective when smaller numbers are involved.
;; It's like trying to approximate the length of a needle
;; with an error margin of plus/minus 1 meter.

;; What about big numbers?

;; scheme@(guile-user)> (my-sqrt 1.0e13)
;; <evaluation doesn't terminate>

;; scheme@(guile-user)> ,trace (my-sqrt 1.0e13)
;; trace: |  (my-sqrt 1.0e13)
;; trace: |  |  (square 1.0)
;; trace: |  |  1.0
;; trace: |  |  (average 1.0 1.0e13)
;; trace: |  |  5000000000000.5
;; trace: |  |  (square 5000000000000.5)
;; trace: |  |  2.5000000000005e25
;; trace: |  |  (average 5000000000000.5 1.9999999999998)
;; trace: |  |  2500000000001.25
;; trace: |  |  (square 2500000000001.25)
;; trace: |  |  6.25000000000625e24
;; <after some more iterations...>
;; trace: |  |  (square 3162277.6640104805)
;; trace: |  |  10000000024299.582
;; trace: |  |  (average 3162277.6640104805 3162277.656326278)
;; trace: |  |  3162277.6601683795
;; trace: |  |  (square 3162277.6601683795)
;; trace: |  |  10000000000000.002
;; trace: |  |  (average 3162277.6601683795 3162277.660168379)
;; trace: |  |  3162277.6601683795
;; trace: |  |  (square 3162277.6601683795)
;; trace: |  |  10000000000000.002
;; trace: |  |  (average 3162277.6601683795 3162277.660168379)
;; trace: |  |  3162277.6601683795
;; trace: |  |  (square 3162277.6601683795)
;; trace: |  |  10000000000000.002

;; "Squeezing infinitely many real numbers into a finite
;; number of bits requires an approximate representation. [...]"

;; — Goldberg, D. (1991) What every computer scientist should know
;; about floating-point arithmetic. ACM computing surveys (CSUR), 23(1), 5-48.

;; Our improve procedure can't improve the guess due to rounding errors
;; that are inherent to floating-point arithmetic. The smallest possible difference
;; between (square guess) and x is larger than our tolerance, 0.001.
;; In other words, due to how the number is encoded in memory,
;; the distance between two consequtive floats in this
;; order of magnitude is larger than 0.001.

;; An interesting observation to be made here is that eliminating these singularities
;; requires a little more involvement from our side than simply messing with the tolerance.
;; To be able to more accurately approximate the square root of small numbers,
;; we can decrease the tolerance, say to 1.0e-7. However, this will mean
;; we now cannot compute the square root of numbers as big as the ones we previously could,
;; due to the rounding errors introduced and how they compare relative to the tolerance.
;; Changing the tolerance doesn't improve our procedure — it shifts the window
;; in the number line in which we can effectively approximate square roots.

;; Let's improve the procedure as instructed by the exercise definition next:

(define (average a b)
  (/ (+ a b) 2))

(define (my-sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- guess (improve guess))) 0.001))
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 1.0))

;; scheme@(guile-user)> (my-sqrt 1.0e13)
;; 3162277.6601683795

;; Fixed!

;; Essentially the only thing of interest is that
;; (- (square guess) x) has changed to
;; (- guess (average guess (/ x guess)))

;; This guarantees that evaluation will always terminate.

;; Of course the underlying precision issues are still present,
;; for example (compare with further below):

;; scheme@(guile-user)> (square (my-sqrt 1.0e-16))
;; 3.8146972656916666e-6

;; Observe that now we call improve twice for each iteration —
;; the second call is hidden inside good-enough?.
;; To fix this, we could assign the return value of improve to a variable
;; and reference it later to call it only once,
;; but we don't know how to do that yet.

;; Lastly, there's a clever simple trick.
;; "[...] in real computers, arithmetic operations are almost always
;; performed with limited precision." What does this mean?
;; It means that we can know for sure that at some point,
;; improve won't change the guess any more.
;; We can do a small modification in good-enough? to achieve
;; the maximum precision available. Instead of defining a tolerance,
;; we can check instead that the guess hasn't changed at all:

(define (my-sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (= guess (improve guess)))
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 1.0))

;; scheme@(guile-user)> (square (my-sqrt 1.0e-16))
;; 1.0000000000000001e-16

