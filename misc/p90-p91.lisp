;;;; Chessboard problems

;;; p90

(defun eight-queens ()
  "Return solutions for the eight queens problem in the form of 8-element lists of numbers from 1 to 8, 
each representing the row of the queen on each column."
  (mapcan #'try-row (range 1 8)))

(defun try-row (row &optional (col 1) visited)
  "Return solutions for the eight queens problem that start with the given row number."
  (if (= col 8)
      (list (list row))
      (mapcan (lambda (n)
                (mapcar (lambda (sol) (cons row sol))
                  (remove-if (lambda (sol) (incompatible-p row sol))
                    (try-row n (1+ col) (cons row visited)))))
              (set-difference (range 1 8) (cons row visited)))))

(defun incompatible-p (row sol)
  "Return T if there is a queen in SOL that is on the same diagonal as ROW (those on the same row are eliminated by TRY-ROW)."
  (do ((up (1- row) (1- up))
       (down (1+ row) (1+ down)))
      ((null sol))
      (when (member (pop sol) (list up down))
        (return T))))
