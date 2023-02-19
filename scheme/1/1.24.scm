;;; Exercise 1.24
;; Modify the timed-prime-test procedure of Exercise 1.22
;; to use fast-prime? (the Fermat method), and test
;; each of the 12 primes you found in that exercise.
;; Since the Fermat test has Θ(log n) growth, how would you
;; expect the time to test primes near 1,000,000 to compare
;; with the time needed to test primes near 1000?
;; Do your data bear this out? Can you explain any discrepancy you find?

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else #f)))

(define (prime? n)
  (fast-prime? n 20)) ; 10-15 iterations are usually sufficient

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

;; scheme@(guile-user)> (search-for-primes 1000000000 1000000181)
;; 1000000007 *** 44200
;; 1000000009 *** 65700
;; 1000000021 *** 59600
;; 1000000033 *** 59400
;; 1000000087 *** 46700
;; 1000000093 *** 61300
;; 1000000097 *** 59700
;; 1000000103 *** 68000
;; 1000000123 *** 61700
;; 1000000181 *** 45600
;; mean: 57190

;; scheme@(guile-user)> (search-for-primes 10000000000 10000000207)
;; 10000000019 *** 86000
;; 10000000033 *** 83400
;; 10000000061 *** 97300
;; 10000000069 *** 84800
;; 10000000097 *** 86100
;; 10000000103 *** 88600
;; 10000000121 *** 88700
;; 10000000141 *** 86900
;; 10000000147 *** 87900
;; 10000000207 *** 89900
;; mean: 80220
;; ratio: 1.402

;; scheme@(guile-user)> (search-for-primes 100000000000 100000000171)
;; 100000000003 *** 93900
;; 100000000019 *** 98100
;; 100000000057 *** 98900
;; 100000000063 *** 102900
;; 100000000069 *** 195500
;; 100000000073 *** 235400
;; 100000000091 *** 114300
;; 100000000103 *** 98800
;; 100000000129 *** 93900
;; 100000000171 *** 104700
;; mean: 123640
;; ratio: 1.541

;; scheme@(guile-user)> (search-for-primes 1000000000000 1000000000193)
;; 1000000000039 *** 190300
;; 1000000000061 *** 193200
;; 1000000000063 *** 194900
;; 1000000000091 *** 185200
;; 1000000000121 *** 183900
;; 1000000000163 *** 227000
;; 1000000000169 *** 181700
;; 1000000000177 *** 180400
;; 1000000000189 *** 187000
;; 1000000000193 *** 178300
;; mean: 190240
;; ratio: 1.538

;; Chicken Scheme time! Check 1.24chicken.scm.
;; Chicken Scheme is fast though... In fact, it's so fast
;; that applying Fermat's test *just* 20 times
;; is so fast that we can't even get any meaningful measurements.
;; And *because* we are now in the Θ(log n) order of growth gang,
;; increasing n also doesn't make a difference.
;; To get more meaningful results I opted to apply Fermat's test
;; for primality 1000 times for each candidate number.
;; mean1: 10.8
;; mean2: 13.3
;; mean3: 13.8
;; mean4: 15.2

;; ratios:
;; 1.234
;; 1.035
;; 1.101

;; Okay... let's do 10000 just to make sure.
;; mean1: 101.9
;; mean2: 128.1
;; mean3: 134.8
;; mean4: 145.1

;; ratios:
;; 1.257
;; 1.052
;; 1.076

;; These are very good results, especially if you consider
;; that there's a call to a procedure generating a rantom integer
;; across the call stack, which is very expensive, in relative terms.
;; Θ(log n) scales very well.

;; If using fast algorithms for modular exponentiation like above
;; and (multiprecision) multiplication, the algorithm is
;; O(k (log n)^2), where k is the number of times
;; we test a random a.

;; Don't forget that this is a probabilistic test!
;; It is proven that there are infinitely many (Fermat)
;; pseudoprimes — numbers that the test flags as prime
;; when they are not, for a given a. What's worse, is there's
;; infinite Carmichaels numbers. These are Fermat liars
;; *regardless* of the choice of a. In practice, more strong
;; probabilistic primality tests are used, such as the
;; Baillie-PSW, the Miller-Rabin, or the Solovay-Strassen.

;; The Fermat test is a bit faster than other candidates, say,
;; Miller-Rabin, but for large numbers the difference is not
;; considerable. While there are better alternatives to the
;; Fermat test, it's used in programs such as Libgcrypt and GMP
;; for a relatively small a before running the Miller-Rabin.
;; Miller-Rabin detects and avoids the Carmichael numbers.

;; Is an a of 20, or 1000 good enough? Well, the probability
;; of getting a wrong result is at most 2^(-a).
