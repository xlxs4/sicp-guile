;;; Exercise 1.28
;; One variant of the Fermat test that cannot be fooled
;; is called the Miller-Rabin test. This starts from an
;; alternate form of Fermat's Little Theorem, which states
;; that if n is a prime number and a is any positive integer
;; less than n, then a raised to the (n - 1) -st power is
;; congruent to 1 modulo n. To test the primality of a
;; number n by the Miller-Rabin test, we pick a random number
;; a < n and raise a to the (n - 1) -st power modulo n using
;; the expmod procedure. However, whenever we perform the
;; squaring step in expmod, we check to see if we have
;; discovered a "nontrivial square root of 1 modulo n,"
;; that is, a number not equal to 1 or n - 1 whose square
;; is equal to 1 modulo n. It is possible to prove that
;; if such a nontrivial square root of 1 exists, then
;; n is not prime. It is also possible to prove that
;; if n is an odd number that is not prime, then,
;; for at least half the numbers a < n, computing a^(n - 1)
;; in this way will reveal a nontrivial square root
;; of 1 modulo n. (This is why the Miller-Rabin test cannot
;; be fooled.) Modify the expmod procedure to signal if it
;; discovers a nontrivial square root of 1, and use this
;; to implement the Miller-Rabin test with a procedure
;; analogous to fermat-test. Check your procedure by
;; testing various known primes and non-primes.
;; Hint: One convenient way to make expmod signal
;; is to have it return 0.

(define (square x) (* x x))

(define (miller-rabin? n)
  (miller-rabin-test (- n 1) n))

(define (miller-rabin-test a n)
  (cond ((= a 0) #t)
        ((= (expmod a (- n 1) n) 1)
         (miller-rabin-test (- a 1) n))
        (else #f)))

(define (non-trivial-sqrt? n m)
  (cond ((= n 1) #f)
        ((= n (- m 1)) #f)
        (else (= (remainder (square n) m) 1))))

(define (mod-check x m)
  (if (non-trivial-sqrt? x m)
      0
      (remainder (square x) m)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (mod-check (expmod base (/ exp 2) m) m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(map miller-rabin? '(4 6 8 10 12 14)) ; (#f #f #f #f #f #f)
(map miller-rabin? '(9 15 21 25 33 45))  ; (#f #f #f #f #f #f)
(map miller-rabin? '(3 5 7 11 13 17))  ; (#t #t #t #t #t #t)
(map miller-rabin? '(561 1105 1729 2465 2821 6601))  ; (#f #f #f #f #f #f)

;; Note that SICP is wrong in the above exercise definition.
;; That is because it uses the wrong definition of a Miller-Rabin
;; witness. It is not true that  if n is an odd number that
;; is not prime, then, for at least half the numbers a < n,
;; computing a^(n - 1) in this way will reveal a nontrivial
;; square root of 1 modulo n. For more information on this,
;; see the following link:
;; https://stackoverflow.com/a/59834347/10772985

