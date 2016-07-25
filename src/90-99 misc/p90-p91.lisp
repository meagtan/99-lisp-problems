;;;; Chessboard problems

;;; p90

(defun eight-queens (&optional (n 8))
  "Return solutions for the eight queens problem in the form of N-element lists of numbers from 1 to N, 
each representing the row of the queen on each column."
  (mapcan (lambda (r) (try-row r n)) (range 1 n)))

(defun try-row (row &optional (n 8) (col 1) visited)
  "Return solutions for the eight queens problem that start with the given row number."
  (if (= col n)
      (list (list row))
      (mapcan (lambda (r)
                (mapcar (lambda (sol) (cons row sol))
                  (remove-if (lambda (sol) (incompatible-p row sol))
                    (try-row r n (1+ col) (cons row visited)))))
              (set-difference (range 1 n) (cons row visited)))))

(defun incompatible-p (row sol)
  "Return T if there is a queen in SOL that is on the same diagonal as ROW (those on the same row are eliminated by TRY-ROW)."
  (do ((up (1- row) (1- up))
       (down (1+ row) (1+ down)))
      ((null sol))
      (when (member (pop sol) (list up down))
        (return T))))

;;; p91

(defun knights-tour (&optional (n 8) (m (* n n)) (visited (list (cons 1 1))))
  "Return all possible ways a knight can move on a NxN chessboard such that every square is visited only once."
  (if (= m 0)
      visited
      (mapcan (lambda (next)
                (knights-tour n (1- m) (cons next visited)))
              (set-difference (next-squares (car visited) n) visited))))

(defun next-squares (sq n)
  "Return the squares that SQ can move to on an NxN chessboard."
  (remove-if (lambda (next) 
               (or (out-of-bounds (car next) n)
                   (out-of-bounds (cdr next) n)))
    (mapcar (lambda (move) 
              (cons (+ (car move) (car sq))
                    (+ (cdr move) (cdr sq))))
      (knight-moves))))

(defun knight-moves ()
  "Return all moves a knight can make on a chessboard."
  '((1 . 2) (2 . 1) (-1 . 2) (2 . -1) (-1 . -2) (-2 . -1) (1 . -2) (-2 . 1)))
