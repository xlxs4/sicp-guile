;;; Exercise 2.29
;; A binary mobile consists of two branches,
;; a left branch and a right branch. Each branch
;; is a rod of a certain length, from which hangs
;; either a weight or another binary mobile.
;; We can represent a binary mobile using compound
;; data by constructing it from two branches
;; (for example, using list):

;; (define (make-mobile left right)
;;   (list left right))

;; A branch is constructed from a length
;; (which must be a number) together with structure,
;; which may be either a number (representing
;; a simple weight) or another mobile:

;; (define (make-branch length structure)
;;   (list length structure))

;; 1. Write the corresponding selectors left-branch
;; and right-branch, which return the branches of a
;; mobile, and branch-length and branch-structure,
;; which return the components of a branch.

;; 2. Using your selectors, define a procedure
;; total-weight that returns the total weight
;; of a mobile.

;; 3. A mobile is said to be balanced if the torque
;; applied by its top-left branch is equal to
;; that applied by its top-right branch
;; (that is, if the length of the left rod
;; multiplied by the weight hanging from that rod
;; is equal to the corresponding product for the
;; right side) and if each of the submobiles
;; hanging off its branches is balanced. Design a
;; predicate that tests whether a binary mobile is
;; balanced.

;; 4. Suppose we change the representation of mobiles
;; so that the constructors are

;; (define (make-mobile left right)
;;   (cons left right))

;; (define (make-branch length structure)
;;   (cons length structure))

;; How much do you need to change your programs
;; to convert to the new representation?

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; > (define my-mobile (make-mobile
;;                      (make-branch 2 3)
;;                      (make-branch 2 3)))
;; > my-mobile
;; '((2 3) (2 3))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

;; > (left-branch my-mobile)
;; '(2 3)
;; > (right-branch my-mobile)
;; '(2 3)

(define (total-weight mobile)
  (cond ((pair? mobile)
         (let ((rb (right-branch mobile)))
           (if (pair? rb)
               (+ (total-weight (branch-structure (left-branch mobile)))
                  (total-weight (branch-structure (right-branch mobile))))
               rb)))
        (else mobile)))

;; > (total-weight my-mobile)
;; 6

(define (balanced? mobile)
  (define (torque branch)
    (* (branch-length branch)
       (total-weight branch)))
  (cond ((pair? mobile)
         (let ((lb (left-branch mobile)))
           (if (pair? lb)
               (= (torque lb)
                  (torque (right-branch mobile)))
               #f)))
        (else #f)))

;; > (balanced? my-mobile)
;; #t

;; If we change the representation of mobiles by
;; redefining the constructors as follows

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;; Then, since we're now using cons instead of list,
;; we only need change some of our selectors.
;; Specifically, instead of cadr, we now need to use cdr.

(define (right-branch mobile) (cdr mobile))
(define (branch-structure branch) (cadr branch))

;; And everything works as intended!
;; Data abstraction is cool.
