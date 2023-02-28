;;; Exercise 2.3
;; Implement a representation for rectangles in a
;; plane. (Hint: You may want to make use of
;; Exercise 2.2.) In terms of your constructors and
;; selectors, create procedures that compute the
;; perimeter and the area of a given rectangle. Now
;; implement a different representation for rectanagles.
;; Can you design your system with suitable abstraction
;; barriers, so that the same perimeter and area
;; procedures will work using either representation?

(define (square x) (* x x))

;; First abstraction barrier.

(define (perimeter-rect r)
  (+ (* (base-rect r) 2)
     (* (height-rect r) 2)))

(define (area-rect r)
  (* (base-rect r)
     (height-rect r)))

;; We need the base and the height, and we can
;; have everything we need with the origin point
;; and the angle by which the rectangle is rotated
;; from the x-axis.

(define (make-rect base height origin rotation)
  (cons (cons base height) (cons origin rotation)))

(define (base-rect r) (car (car r)))
(define (height-rect r) (cdr (car r)))

(define (origin-rect r) (car (cdr r)))
(define (rotation-rect r) (cdr (cdr r)))

;; There's another, more complicated way to create
;; a rectangle only using the base, as a segment
;; this time, and the height of the end of the other
;; side segment, perpendicular to (i.e. from)
;; the base segment.

(define (make-rect x-segment height)
  (let ((p1 (start-segment x-segment))
        (p2 (end-segment x-segment)))
    (let ((x1 (x-point p1))
          (y1 (y-point p1))
          (x2 (x-point p2))
          (y2 (y-point p2)))
      (let ((rot-angle (atan (/ (- y2 y1)
                                (- x2 x1)))))
        (let ((x2 (- x1 (* height (sin rot-angle))))
              (y2 (+ y1 (* height (cos rot-angle)))))
          (cons x-segment
                (make-segment
                 p1
                 (make-point x2 y2))))))))

;; Now we need to change our selectors too,
;; since to calculate the area and perimeter of
;; a rectangle, we need scalars, not vectors.

(define (euclid-norm s)
  (let ((p1 (start-segment s))
        (p2 (end-segment s)))
    (let ((x1 (x-point p1))
          (y1 (y-point p1))
          (x2 (x-point p2))
          (y2 (y-point p2)))
      (sqrt (+ (square (- x1 x2))
               (square (- y1 y2)))))))

(define (base-rect r)
  (euclid-norm (base-segment r)))

(define (height-rect r)
  (euclid-norm (left-segment r)))

(define (base-segment r) (car r))
(define (left-segment r) (cdr r))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

;; > (define r1 (make-rect 5 4 (make-point 1 1) 0.2))
;; > r1
;; '((5 . 4) (1 . 1) . 0.2)

;; > (perimeter-rect r1)
;; 18
;; > (area-rect r1)
;; 20

;; > (define r2 (make-rect
;;               (make-segment
;;                (make-point 1 1)
;;                (make-point 2 2))
;;               5))
;; > r2
;; '(((1 . 1) 2 . 2) (1 . 1) -2.5355339059327373 . 4.535533905932738)

;; > (perimeter-rect r2)
;; 12.82842712474619
;; > (area-rect r2)
;; 7.0710678118654755

;; No contract violation! Our abstraction barriers hold ;)
