;;; Exercise 2.42
;; The "eight-queens puzzle" asks how to place
;; eight queens on a chessboard so that no queen
;; is in check from any other (i.e., no two queens
;; are in the same row, column, or diagonal).
;; One possible solution is shown in Figure 2.8.
;; One way to solve the puzzle is to work across
;; the board, placing a queen in each column.
;; Once we have placed k - 1 queens, we must place
;; the kth queen in a position where it does not
;; check any of the queens already on the board.
;; We can formulate this approach recursively:
;; Assume that we have already generated the
;; sequence of all possible ways to place k - 1
;; queens in the first k - 1 columns of the board.
;; For each of these ways, generate an extended
;; set of positions by placing a queen in each row
;; of the kth column. Now filter these, keeping only
;; the positions for which the queen in the kth
;; column is safe with respect to the other queens.
;; This produces the sequence of all ways to place k
;; queens in the first k columns. By continuing this
;; process, we will produce not only one solution,
;; but all solutions to the puzzle.

;; [ ] [ ] [ ] [ ] [ ] [Q] [ ] [ ]
;; [ ] [ ] [Q] [ ] [ ] [ ] [ ] [ ]
;; [Q] [ ] [ ] [ ] [ ] [ ] [ ] [ ]
;; [ ] [ ] [ ] [ ] [ ] [ ] [Q] [ ]
;; [ ] [ ] [ ] [ ] [Q] [ ] [ ] [ ]
;; [ ] [ ] [ ] [ ] [ ] [ ] [ ] [Q]
;; [ ] [Q] [ ] [ ] [ ] [ ] [ ] [ ]
;; [ ] [ ] [ ] [Q] [ ] [ ] [ ] [ ]
;;
;; Figure 2.8: A solution to the eight-queens puzzle.

;; We implement this solution as a procedure queens,
;; which returns a sequence of all solutions to the
;; problem of placing n queens on an n ✕ n chessboard.
;; queens has an internal procedure queen-cols
;; that returns the sequence of all ways to place queens
;; in the first k columns of the board.

;; (define (queens board-size)
;;   (define (queen-cols k)
;;     (if (= k 0)
;;         (list empty-board)
;;         (filter
;;          (lambda (positions)
;;            (safe? k positions))
;;          (flatmap
;;           (lambda (rest-of-queens)
;;             (map (lambda (new-row)
;;                    (adjoin-position
;;                     new-row
;;                     k
;;                     rest-of-queens))
;;                  (enumerate-interval
;;                   1
;;                   board-size)))
;;           (queen-cols (- k 1))))))
;;   (queen-cols board-size))

;; In this procedure rest-of-queens is a way to place
;; k - 1 queens in the first k - 1 columns, and new-row
;; is a proposed row in which to place the queen for
;; the kth column. Complete the program by implementing
;; the representation for sets of board positions,
;; including the procedure adjoin-position, which adjoins
;; a new row-column position to a set of positions,
;; and empty-board, which represents an empty set of positions.
;; You must also write the procedure safe?, which determines
;; for a set of positions, whether the queen in the kth
;; column is safe with respect to the others. (Note that we
;; need only check whether the new queen is safe — the other
;; queens are already guaranteed safe with respect to each other.)

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (make-position row col)
  (list row col))

(define (position-row pos)
  (car pos))

(define (position-col pos)
  (cadr pos))

(define empty-board '())

(define (adjoin-position row col positions)
  (cons (make-position row col) positions))

;; A position with row i and column j can be represented
;; as (i,j). To determine if a Queen at a position m (Qm)
;; checks a Queen at position n (Qn), we need to examine
;; the horizontal line and the two diagonals, one beginning
;; from Qm upwards, and the other beginning from Qm and
;; going downwards. Since we're placing each queen vertically
;; strictly one square right to the last one (Qnj = Q_{n-1}j + 1),
;; we only need to check these three lines. Also, since we're
;; placing each queen on a different column, there's no need
;; to check for the vertical line.

;; So, Qm checks Qn if:
;; 1) on the straight line, if Qmi == Qni
;; 2) on the diagonals
;;   a) on the upward one, if Qmi + n - m == Qni
;;   b) on the downward one, if Qmi - n - m == Qni
;;
;; The diagonals check can be simplified to one check:
;; if abs(Qmi - Qni) == abs(Qmj - Qnj)

;; (safe? qn) needs to test (checks? qm qn) for
;; m = n - 1, n - 2, ... 1
;; So, (safe? q qs) needs to test (checks? qi q)
;; for every qi in qs. With that in mind:

(define (safe? k positions)
  (define (two-queens-safe? q1 q2)
    (define (same-row? pos1 pos2) ; abstraction barriers!
      (= (position-row pos1) (position-row pos2)))
    (define (same-diag? pos1 pos2)
      (= (abs (- (position-row pos1) (position-row pos2)))
         (abs (- (position-col pos1) (position-col pos2)))))
      (not (or (same-row? q1 q2)
               (same-diag? q1 q2))))
  (let ((kpos (get-pos-by-col k positions)))
    (accumulate (lambda (pos curr-ans)
                  (and (two-queens-safe? kpos pos)
                       curr-ans))
                #t
                (remove kpos positions))))

(define (get-pos-by-col col coordinates)
  (cond ((null? coordinates)
         '())
        ((= col (position-col (car coordinates)))
         (car coordinates))
        (else
         (get-pos-by-col col (cdr coordinates)))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row
                    k
                    rest-of-queens))
                 (enumerate-interval
                  1
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; > (length (queens 8))
;; 92

;; It works!
;;... can we do better?

;; Well, we actually don't need to pas k to safe?
;; And it can generate an iterative process too.

(define (safe? positions)
  (define (two-queens-safe? q1 q2)
    (define (same-row? pos1 pos2) ; abstraction barriers!
      (= (position-row pos1) (position-row pos2)))
    (define (same-diag? pos1 pos2)
      (= (abs (- (position-row pos1) (position-row pos2)))
         (abs (- (position-col pos1) (position-col pos2)))))
      (not (or (same-row? q1 q2)
               (same-diag? q1 q2))))
  (define (iter pos positions)
    (if (null? positions)
        #t
        (if (two-queens-safe? pos (car positions))
            (iter pos (cdr positions))
            #f)))
  (iter (car positions) (cdr positions)))


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row
                    k
                    rest-of-queens))
                 (enumerate-interval
                  1
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; > (length (queens 8))
;; 92

;; And as a bonus ;)
;; > (length (queens 12))
;; 14200
