;;; Exercise 1.26
;; Louis Reasoner is having great difficulty
;; doing Exercise 1.24. His fast-prime? test seems to run
;; more slowly than his prime? test. Louis calls his friend
;; Eva Lu Ator over to help. When they examine Louis's code,
;; they find that he has rewritten the expmod procedure
;; to use an explicit multiplication, rather than calling square:

;; (define (expmod base exp m)
;;   (cond ((= exp 0) 1)
;;         ((even? exp)
;;          (remainder
;;           (* (expmod base (/ exp 2) m)
;;              (expmod base (/ exp 2) m))
;;           m))
;;         (else
;;          (remainder
;;           (* base
;;              (expmod base (- exp 1) m))
;;           m))))

;; "I don't see what difference that could make," says Louis.
;; "I do." says Eva. "By writing the procedure like that,
;; you have transfored the Θ(log n) process into a Θ(n) process."
;; Explain.

;;; Answer:
;; The difference lies between

;; (square (expmod base (/ exp 2) m))

;; (*
;;  (expmod base (/ exp 2) m)
;;  (expmod base (/ exp 2) m))

;; Because the interpreter is using applicative-order evaluation,
;; the call to expmod is evaluated first, and then the number
;; is fed to *. Louis's version evaluates expmod twice.
;; This eliminates all the benefits we get from using the fast
;; exponentiation algorithm, since it halves the exponent
;; when it is even. We now got double the work to do, so
;; the new order of growth becomes Θ(log 2^n) = Θ(n log2) = Θ(n).
