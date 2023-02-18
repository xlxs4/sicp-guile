;;; Exercise 1.13
;; Prove that Fib(n) is the closest integer to φ^n / sqrt(5), where
;; φ = (1 + sqrt(5)) / 2. Hint: Let ψ = (1 - sqrt(5)) / 2.
;; Use induction and the definition of the Fibonacci numbers (see 1.2.2)
;; to prove that Fib(n) = (φ^n - ψ^n) / sqrt(5).

;;; Answer:
;; Let's start from the hint. We need to prove that
;; Fib(n) = (φ^n - ψ^n) / sqrt(5). We also need to do that using induction,
;; therefore the base case is:
;; Fib(0) = (φ^0 - ψ^0) / sqrt(5) ^ Fib(1) = (φ - ψ) / sqrt(5)
;; we get that Fib(0) = 0 ^ Fib(1) = 1, which agrees
;; with the definition of the Fibonacci numbers.

;; Let's move on to the more interesting part of the proof by induction:
;; Let k be a natural number. If Fib(k) = (φ^k - ψ^k) / sqrt(5) and
;; Fib(k + 1) = (φ^(k + 1) - ψ^(k + 1)) / sqrt(5) are both true, then
;; it must hold that Fib(k + 2) = (φ^(k + 2) - ψ^(k + 2)) / sqrt(5).
;; Using the definition of the Fibonacci numbers,
;; we can rewrite what we want to prove as:
;; Fib(k) + Fib(k + 1) = (φ^(k + 2) - ψ^(k + 2)) / sqrt(5).

;; We can now use our hypotheses for Fib(k) and Fib(k + 1) to substitute:
;; ((φ^k - ψ^k) / sqrt(5)) + (φ^(k + 1) - ψ^(k + 1)) / sqrt(5) =
;; (φ^(k + 2) - ψ^(k + 2)) / sqrt(5). Simplifying:
;; (φ^k (φ + 1) - ψ^k (ψ + 1)) / sqrt(5) = (φ^(k + 2) - ψ^(k + 2)) / sqrt(5).
;; φ^k (φ + 1) - ψ^k (ψ + 1) = φ^(k + 2) - ψ^(k + 2).

;; We know that φ^2 = φ + 1, since φ is the only positive solution
;; to the equation x^2 = x + 1. ψ is similarly the only negative solution.
;; (See page 38.) Therefore, we get:
;; φ^k · φ^2 - ψ^k · ψ^2 = φ^(k + 2) - ψ^(k + 2).
;; φ^(k + 2) - ψ^(k + 2) = φ^(k + 2) - ψ^(k + 2)
;; So we have proven that Fib(n) = (φ^n - ψ^n) / sqrt(5).
;; Finally, we want to use this to prove the original statement —
;; that Fib(n) is the closest integer to φ^n / sqrt(5).

;; In other words, we want to prove that
;; | Fib(n) - φ^n / sqrt(5) | < 1/2.
;; We can use the statement that we have now proven to rewrite as:
;; | (φ^n - ψ^n) / sqrt(5) - φ^n / sqrt(5) | < 1/2. Simplifying:
;; | ψ^n | / sqrt(5) < 1/2
;; We can then substitute using the definition of ψ:
;; | ((1 - sqrt(5)) / 2)^n | /sqrt(5) < 1/2. Simplifying:
;; ((sqrt(5) - 1) / 2)^n / sqrt(5) < 1/2.

;; We know that sqrt(9) = 3, so sqrt(5) < 3. Therefore:
;; We can subtract 1 to get sqrt(5) - 1 < 2,
;; divide by 2 to get (sqrt(5) - 1) / 2 < 1,
;; raise to n to get ((sqrt(5) - 1) / 2)^n < 1^n,
;; multiply by 1 / sqrt(5) to finally get
;; ((sqrt(5) - 1) / 2)^n / sqrt(5) < 1 / sqrt(5).

;; We know that 1 / sqrt(5) < 1/2 since sqrt(5) > 2, if sqrt(4) = 2.
;; Therefore, ((sqrt(5) - 1) / 2)^n / sqrt(5) < 1/2.
;; QED.

