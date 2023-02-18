(import (chicken time))

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
	 n)
	((divides? test-divisor n)
	 test-divisor)
	(else (find-divisor
		n
		(+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (cpu-time)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (cpu-time)
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

(search-for-primes 10000000000 10000000207)
(search-for-primes 100000000000 100000000151)
(search-for-primes 1000000000000 1000000000193)
(search-for-primes 10000000000000 10000000000293)
