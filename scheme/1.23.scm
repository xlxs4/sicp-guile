;;; Exercise 1.23
;; The smallest-divisor procedure shown at the start of this section
;; does lots of needless testing: After it checks to see
;; if the number is divisible by 2 there is no point in
;; checking to see if it is divisible by any larger even numbers.
;; This suggests that the values used for test-divisor
;; should not be 2, 3, 4, 5, 6, ..., but rather 2, 3, 5, 7, 9, ....
;; To implement this change, define a procedure next
;; that returns 3 if its input is equal to 2
;; and otherwise returns its input plus 2.
;; Modify the smallest-divisor procedure to use (next test-divisor)
;; instead of (+ test-divisor 1). With timed-prime-test incorporating
;; this modified version of smallest-divisor, run the test
;; for each of the 12 primes found in Exercise 1.22.
;; Since this modification halves the number of test steps,
;; you should expect it to run about twice as fast.
;; Is this expectation confirmed? If not, what is the observed ratio
;; of the speeds of the two algorithms, and how do you
;; explain the fact that it is different from 2?

(define (square x) (* x x))

;; The required changes are pretty self-explanatory.

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor))))) ; <next> used here

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (get-internal-run-time)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (get-internal-run-time)
                         start-time))))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (search-for-primes start end)
  (define (iter n)
    (cond ((<= n end)
           (timed-prime-test n) (iter (+ n 2)))
          (else
           (newline))))
  (if (even? start)
      (iter (+ 1 start))))

;; scheme@(guile-user)> (search-for-primes 10000000000 10000000207)
;; 10000000019 *** 2158300
;; 10000000033 *** 2371500
;; 10000000061 *** 2124400
;; 10000000069 *** 2123000
;; 10000000097 *** 2801300
;; 10000000103 *** 2173400
;; 10000000121 *** 2184200
;; 10000000141 *** 2196700
;; 10000000147 *** 2122300
;; 10000000207 *** 2136600
;; mean: 2046870
;; previous version mean: 4345780
;; ratio: 0.471

;; scheme@(guile-user)> (search-for-primes 100000000000 100000000171)
;; 100000000003 *** 6861500
;; 100000000019 *** 7564200
;; 100000000057 *** 7140300
;; 100000000063 *** 7168600
;; 100000000069 *** 7062900
;; 100000000073 *** 6735700
;; 100000000091 *** 8056800
;; 100000000103 *** 6904000
;; 100000000129 *** 7958600
;; 100000000171 *** 7882100
;; mean: 7333470
;; previous version mean: 15374640
;; ratio: 0.477

;; scheme@(guile-user)> (search-for-primes 1000000000000 1000000000193)
;; 1000000000039 *** 24032200
;; 1000000000061 *** 23117600
;; 1000000000063 *** 23139000
;; 1000000000091 *** 22907800
;; 1000000000121 *** 22159300
;; 1000000000163 *** 22349300
;; 1000000000169 *** 22606000
;; 1000000000177 *** 22686000
;; 1000000000189 *** 22451000
;; 1000000000193 *** 21931900
;; mean: 22738010
;; previous version mean: 42893800
;; ratio: 0.53

;; Accounting for the little bit of noise, it does run approximately twice as fast.

;; Let's try Chicken Scheme next (1.23chicken.scm).
;; We get:

;; mean1: 3.7
;; previous mean1: 6.5
;; ratio: 0.569

;; mean2: 12.7
;; previous mean2: 19.6
;; ratio: 0.645

;; mean3: 38.5
;; previous mean3: 59.3
;; ratio: 0.64

;; mean4: 118.4
;; previous mean4: 184.7
;; ratio: 0.641

;; Huh, that's... interesting. Well, since I got myself into this,
;; let's first try to hypothesize why this happens in Chicken Scheme
;; and try to make the new algorithm faster, then we can think about
;; why Guile Scheme doesn't care :P

;; We can remove the if special form completely, since we're only looking for primes.

;; (define (next n) (+ n 2))

;; (define (smallest-divisor n)
;;   (find-divisor n 3))

;; And here's what we get:

;; mean1: 3.8
;; mean2: 12.6
;; mean3: 35.6
;; mean4: 115.3

;; almost nothing... let's remove the function call altogether
;; by having the code inline inside find-divisor

;; (define (find-divisor n test-divisor)
;;   (cond ((> (square test-divisor) n) n)
;;         ((divides? test-divisor n) test-divisor)
;;         (else (find-divisor n (+ test-divisor 2)))))

;; mean1: 3.2
;; ratio: 0.492

;; mean2: 10.89
;; ratio: 0.555

;; mean3: 31
;; ratio 0.522

;; mean4: 97.7
;; ratio: 0.528

;; We did better now, getting close to the theoretical 1/2 ratio.

;; TODO: figure out why Guile was doing better out of the box.
;; TODO: some other people online also observe differences
;; (I guess they use MIT Scheme instead)

