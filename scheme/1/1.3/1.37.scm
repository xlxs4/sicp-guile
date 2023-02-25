;;; Exercise 1.37
;; 1. An infinite continued fraction is an expression
;; of the form

;;                N1
;; f = -------------------------
;;                   N2
;;      D1 + ------------------
;;                     N3
;;           D2 + -----------
;;                  D3 + ...

;; As an example, one can show that the infinite
;; continued fraction expansion with the Ni and the Di
;; all equal to 1 produces 1/φ, where φ is the golden
;; ratio (described in 1.2.2). One way to approximate
;; an infinite continued fraction is to truncate the
;; expansion after a given number of terms. Such a
;; truncation — a so-called finite continued fraction
;; *k-term finite continued fraction* — has the form

;;          N1
;; ---------------------
;;           N2
;;   D1 + ------------
;;              Nk
;;        ⠑ + ------
;;              Dk

;; Suppose that n and d are procedures of one argument
;; (the term index i) that return the Ni and Di of the
;; terms of the continued fraction. Define a procedure
;; cont-frac such that evaluating (cont-frac n d k)
;; computes the value of the k-term finite continued
;; fraction. Check your procedure by approximating 1/φ
;; using

;; (cont-frac (lambda (i) 1.0)
;;            (lambda (i) 1.0)
;;            k)

;; for successive values of k. How large must you make
;; k in order to get an approximation that is accurate
;; to 4 decimal places?

;; 2. If your cont-frac procedure generates a recursive
;; process, write one that generates an iterative
;; process. If it generates an iterative process,
;; write one that generates a recursive process.

(define (cont-frac n d k)
  (define (recur i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))

(define (cont-frac n d k)
  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (- i 1) (/ (n i) (+ (d i) acc)))))
  (iter k 0))

(define (approximate-phi k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

;; 1/φ is ≅ 0.6180339887498

;; scheme@(guile-user)> (approximate-phi 10)
;; 0.6179775280898876
;; scheme@(guile-user)> (approximate-phi 11)
;; 0.6180555555555556
