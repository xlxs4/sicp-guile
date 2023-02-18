;;; Exercise 1.22
;; Most Lisp implementations include a primitive called runtime
;; that returns an integer that specifies the amount of time
;; the system has been running (measured, for example, in microseconds).
;; The following timed-prime-test procedure, when called with an integer n,
;; prints n and checks to see if n is prime. If n is prime,
;; the procedure prints three asterisks followed by the amount of time
;; used in performing the test.

;; (define (timed-prime-test n)
;;   (newline)
;;   (display n)
;;   (start-prime-test n (runtime)))

;; (define (start-prime-test n start-time)
;;   (if (prime? n)
;;       (report-prime (- (runtime)
;;                        start-time))))

;; (define (report-prime elapsed-time)
;;   (display " *** ")
;;   (display elapsed-time))

;; Using this procedure, write a procedure search-for-primes
;; that checks the primality of consecutive odd integers
;; in a specified range. Use your procedure to find the three smallest primes
;; larger than 1000; larger than 10,000; larger than 100,000; larger than 1,000,000.
;; Note the time needed to test each prime. Since the testing algorithm
;; has order of growth of Θ(sqrt(n)), you should expect that
;; testing for primes around 10,000 should take about sqrt(10) times
;; as long as testing for primes around 1000.
;; Do your timing data bear this out?
;; How well do the data for 100,000 and 1,000,000 support the Θ(sqrt(n)) prediction?
;; Is your result compatible with the notion that programs on your machine
;; run in time proportional to the number of steps required for the computation?

;;; Answer:
;; The procedure runtime exists in MIT Scheme, but not Guile.
;; We can use get-internal-run-time instead.
;; I have changed some things to get a less-cluttered output.

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

;; Let's search-for-primes. Even numbers can't be primes!

(define (search-for-primes start end)
  (define (iter n)
    (cond ((<= n end)
           (timed-prime-test n) (iter (+ n 2)))
          (else
           (newline))))
  (if (even? start)
      (iter (+ 1 start))))

;; I got 10 numbers for each range to supress noise.

;; scheme@(guile-user)> (search-for-primes 1000 1061)
;; 1009 *** 1800
;; 1013 *** 1800
;; 1019 *** 1700
;; 1021 *** 1800
;; 1031 *** 1700
;; 1033 *** 1800
;; 1039 *** 2000
;; 1049 *** 1800
;; 1051 *** 1900
;; 1061 *** 1800
;; mean: 1810

;; scheme@(guile-user)> (search-for-primes 10000 10093)
;; 10007 *** 6900
;; 10009 *** 8200
;; 10037 *** 6800
;; 10039 *** 8100
;; 10061 *** 6900
;; 10067 *** 7100
;; 10069 *** 7000
;; 10079 *** 6800
;; 10091 *** 6800
;; 10093 *** 6900
;; mean: 7150
;; ratio with previous mean: 3.95

;; scheme@(guile-user)> (search-for-primes 100000 100151)
;; 100003 *** 12800
;; 100019 *** 12500
;; 100043 *** 12400
;; 100049 *** 15500
;; 100057 *** 15300
;; 100069 *** 28600
;; 100103 *** 13200
;; 100109 *** 13300
;; 100129 *** 13200
;; 100151 *** 13100
;; mean: 14990
;; ratio with previous mean: 2.09

;; scheme@(guile-user)> (search-for-primes 1000000 1000151)
;; 1000003 *** 64400
;; 1000033 *** 64400
;; 1000037 *** 64700
;; 1000039 *** 65500
;; 1000081 *** 64300
;; 1000099 *** 64300
;; 1000117 *** 64400
;; 1000121 *** 64600
;; 1000133 *** 64200
;; 1000151 *** 64300
;; mean: 64510
;; ratio with previous mean: 4.30

;; Our computers have gotten faster!
;; Let's crank the numbers up to get less noisy results:

;; scheme@(guile-user)> (search-for-primes 1000000000 1000000181)
;; 1000000007 *** 1351900
;; 1000000009 *** 1422500
;; 1000000021 *** 1322000
;; 1000000033 *** 1828400
;; 1000000087 *** 1470900
;; 1000000093 *** 1435400
;; 1000000097 *** 1225800
;; 1000000103 *** 1737500
;; 1000000123 *** 1284800
;; 1000000181 *** 1226200
;; mean: 1430540

;; scheme@(guile-user)> (search-for-primes 10000000000 10000000207)
;; 10000000019 *** 4103100
;; 10000000033 *** 4298100
;; 10000000061 *** 4549800
;; 10000000069 *** 4616000
;; 10000000097 *** 4497000
;; 10000000103 *** 5112800
;; 10000000121 *** 4025400
;; 10000000141 *** 3633600
;; 10000000147 *** 4460000
;; 10000000207 *** 4162000
;; mean: 4345780
;; ratio with previous mean: 3.03

;; scheme@(guile-user)> (search-for-primes 100000000000 100000000171)
;; 100000000003 *** 18692000
;; 100000000019 *** 17899200
;; 100000000057 *** 14135300
;; 100000000063 *** 13693200
;; 100000000069 *** 17877200
;; 100000000073 *** 14089000
;; 100000000091 *** 14552800
;; 100000000103 *** 14433900
;; 100000000129 *** 13759200
;; 100000000171 *** 14614500
;; mean: 15374640
;; ratio with previous mean: 3.53

;; scheme@(guile-user)> (search-for-primes 1000000000000 1000000000193)
;; 1000000000039 *** 41749400
;; 1000000000061 *** 45788800
;; 1000000000063 *** 45485800
;; 1000000000091 *** 42953600
;; 1000000000121 *** 42391100
;; 1000000000163 *** 39431500
;; 1000000000169 *** 41822100
;; 1000000000177 *** 42585400
;; 1000000000189 *** 43706800
;; 1000000000193 *** 43023500
;; mean: 42893800
;; ratio with previous mean: 2.79

;; For less variance, I also implemented the procedure in Chicken Scheme.
;; This was done for the sole reason that Chicken Scheme features a compiler.
;; Instead of get-internal-run-time, Chicken has cpu-time.
;; For bigger numbers, in Chicken scheme we get the following
;; successive ratios (check the 1.22chicken.scm file):
;; 3.16
;; 3.15
;; 3.16

;; sqrt(10) = 3.16
;; This seems to confirm that the testing algorithm
;; has order of growth of Θ(sqrt(n)).

