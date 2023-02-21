;;; Exercise 1.25
;; Alyssa P. Hacker complains that we went to
;; a lot of extra work in writing expmod. After all, she says,
;; since we already know how to compute exponentials,
;; we could have simply written

;; (define (expmod base exp m)
;;   (remainder (fast-expt base exp) m))

;; Is she correct? Would this procedure serve as well
;; for our prime tester? Explain.

;;; Answer:
;; Let's compare. We have these two following versions of expmod:

(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0)
         1)
        ((even? n)
         (square (fast-expt b (/ n 2))))
        (else
         (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;; and the second version:

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

;; Let's take a look at each trace first:

;; scheme@(guile-user)> ,trace (remainder (fast-expt 5 30) 30)
;; <truncated>
;; trace: |  (fast-expt 5 30)
;; trace: |  |  (fast-expt 5 15)
;; trace: |  |  |  (fast-expt 5 14)
;; trace: |  |  |  |  (fast-expt 5 7)
;; trace: |  |  |  |  |  (fast-expt 5 6)
;; trace: |  |  |  |  |  |  (fast-expt 5 3)
;; trace: |  |  |  |  |  |  |  (fast-expt 5 2)
;; trace: |  |  |  |  |  |  |  |  (fast-expt 5 1)
;; trace: |  |  |  |  |  |  |  |  |  (fast-expt 5 0)
;; trace: |  |  |  |  |  |  |  (square 5)
;; trace: |  |  |  |  |  (square 125)
;; trace: |  |  |  (square 78125)
;; trace: |  (square 30517578125)
;; trace: |  931322574615478515625
;; trace: |  25

;; scheme@(guile-user)> ,trace (expmod 5 30 30)
;; <truncated>
;; trace: |  (expmod 5 30 30)
;; trace: |  |  (expmod 5 15 30)
;; trace: |  |  |  (expmod 5 14 30)
;; trace: |  |  |  |  (expmod 5 7 30)
;; trace: |  |  |  |  |  (expmod 5 6 30)
;; trace: |  |  |  |  |  |  (expmod 5 3 30)
;; trace: |  |  |  |  |  |  |  (expmod 5 2 30)
;; trace: |  |  |  |  |  |  |  |  (expmod 5 1 30)
;; trace: |  |  |  |  |  |  |  |  |  (expmod 5 0 30)
;; trace: |  |  |  |  |  |  |  |  (square 5)
;; trace: |  |  |  |  |  |  (square 5)
;; trace: |  |  |  |  (square 5)
;; trace: |  |  (square 5)
;; trace: |  |  25
;; trace: |  25

;; I said "Hey! What's going on?" — 4 Non Blondes
;; ... So we can see that an obvious difference is that
;; Alyssa's version has a chain of successive squaring.
;; Then, it calls remainder on the huge resulting square.
;; This means that it produces intermidiate numbers of a^m size.
;; The number of digits for the result of (fast-expt base exp)
;; will be log10(base^exp) + 1 = exp ✕ log10(base) + 1.

;; Our expmod version however has the remainder operation
;; inside the procedure definition. What this does is essentially
;; keeping the square operand strictly less than
;; the number tested for primality.

;; Let's try to get some metrics now. See 1.25chicken.scm.

;; mean1: 0.8

;; mean2: 41.1
;; ratio: 51.375

;; mean3: 2248
;; ratio: 54.695

;; If I run the code with our expmod and these parameters
;; i.e. applying Fermat's test for primality only 20 times
;; and in such small ranges, the code runs so fast I can't
;; even time it that way. Good job, Alyssa. Good job.

;; What's wrong with working with a big number?
;; The number is represented somehow in memory.
;; If the number size exceeds the size of the biggest internal
;; representation of an integer as available to the programming language,
;; things break. You might get an overflow, resulting in either a crash
;; or in undefined behavior. Or the number might be automatically
;; converted to a larger type such as a floating-point number,
;; which can represent a wider range of values but suffers
;; problems related with the limited precision inherent to
;; floating-point arithmetic. In Guile Scheme, there are no
;; (such) size restrictions when working with integers.

;; Specifically, Guile Scheme levarages GNU GMP, the GNU
;; Multi-Precision library to support working with these bigger numbers,
;; the so-called bignums. Since we call square, we're interested
;; in how integer multiplication is done. GMP uses polyalgorithms
;; for multiplication. The "NxN" case, i.e. as in square, follows
;; a different strategy than the "NxM" case. We're interested in
;; the former.

;; Which multiplication algorithm gets employed has to do with
;; the size of the number involved. From smallest to largest, the
;; algorithms employed are:
;; 1. Basecase Multiplication
;;    O(N*M), N*N case takes roughly half time, although it's still O(N^2).
;; 2. Karatsuba Multiplication
;;    Asymptotically an O(N^1.585) (N^(log(3)/log(2))).
;;    Note that while each algorithm further down the list is an
;;    asymptotic improvement from the previous one, it introduces
;;    additional overhead, so using it is justified for larger sizes.
;;    GMP uses some size thresholds to determine which algorithm to use.
;; 3. Toom 3-Way Multiplication
;;    Asymptotically an O(N^1.465) (N^(log(5)/log(3))).
;; 4. Toom 4-Way Multiplication
;;    Asymptotically an O(N^1.404) (N^(log(7)/log(4))).
;; 5. Higher degree Toom'n'half
;;   a. Toom-6'n'half
;;   b. Toom-8'n'half
;; 6. FFT Multiplication
;;    Based on the work of Schonhage and Strassen. Essentially
;;    O(N^(k/(k-1))), where each successive k is an asymptotic improvement,
;;    but overheads mean each is only faster at bigger and bigger sizes.

;; So yes, some people care about doing arithmetic operations on large-size
;; integer operands while retaining the needed precision, and this is
;; something Guile Scheme can do thanks to GMP, but it's still way slower.
;; Also you should take heed of the space complexity. These bignums
;; have to be internally represented in memory, and that can be the
;; ultimate hard limit on what can and cannot be done on your machine.
;; Although, as Gerald Jay Sussman put it in
;; "We Really Don't Know How to Compute", memory is free, so I don't know.

;; Still, you have to appreciate how much collective effort has been put
;; behind things GMP, because then you start to understand that
;; memory and speed might not be the only issue when you have to resort
;; to dealing with humongus numbers. For example, taken from the
;; "GNU Guile 3.0.6" announcement on the Guile Scheme mailing list:
;; > [...] Guile is usually built to dynamically link to libgmp.
;; In this configuration, any other user of GMP in the process uses
;; the same libgmp instance, with the same shared state.
;;
;; > An important piece of shared state is the GMP allocator,
;; responsible for allocating storage for the digits of large integers.
;; For Guile it's most efficient to install libgc as the GMP allocator.
;; That way Guile doesn't need to install finalizers, which have
;; significant overhead, to free GMP values when Guile bignums
;; are collected. Using libgc to allocate digits also allows Guile's GC
;; to adequately measure the memory cost of these values.
;;
;; > However, if the Guile process is linked to some other user of GMP,
;; then probably the references from the other library to GMP values
;; aren't visible to the garbage collector. In this case libgc could
;; prematurely collect values from that other GMP user.
;;
;; > This isn't theoretical, sadly: it happens for Guile-GnuTLS. [...]

