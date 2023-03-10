;;; Exercise 2.35
;; Redefine count-leaves from 2.2.2 as an accumulation:

;; (define (count-leaves t)
;;   (accumulate <??> <??> (map <??> <??>)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

;; Using enumerate-tree this becomes very straightforward.

(define (count-leaves t)
  (accumulate (lambda (x y)
                (+ (length x) y))
              0
              (map enumerate-tree t)))

(define (identity x) x)

;; Otherwise, we've seen in Exercise 2.33 that
;; to define length in terms of accumulate we
;; used (lambda (x y) (+ y 1)), which consumes
;; a sequence element to increment the result by 1.
;; The logic remains the same, but now we need to
;; check if we're dealing with a nested structure
;; or not. If yes, we need to recurse deeper into
;; the tree. Doing so, passing (map identity t) is
;; of course redundant, and we can simply make do
;; with passing t instead.

(define (count-leaves t)
  (accumulate
   (lambda (x y)
     (+ y
        (cond ((pair? x)
            (count-leaves x))
            (else 1))))
   0
   (map identity t)))
