;;;; Sudoku solver

(defun solve-sudoku (array &optional (spot (next-undefined-spot array)))
  "Solve a valid Sudoku puzzle in-place, represented by a 9x9 array with elements either an integer between 1 and 9 or NIL."
  (if (null spot)
      array
      (mapcan (lambda (n)
                (setf (aref array (car spot) (cdr spot)) n)
                (solve-sudoku array (next-undefined-spot array spot)))
              (set-difference (range 1 9) ;p22
                (union (row-defined-spots array spot)
                       (col-defined-spots array spot)
                       (sqr-defined-spots array spot))))))

(defun next-undefined-spot (array &optional (start (cons 0 0)))
  "Return the next spot of ARRAY, read from left to right and from top to bottom, that comes after START and is NIL."
  (do ((row (car start)) 
       (col (cdr start)))
      ((= row (array-dimension array 0)))
      (unless (aref array row col)
        (return (cons row col)))
      (if (= (incf col) (array-dimension array 1))
          (setf row (1+ row) col 0))))

(defun row-defined-spots (array spot)
  "Return the numbers on the same row as SPOT in ARRAY that are not NIL."
  (loop for col to (array-dimension array 1)
        for row = (car spot)
        if (aref array row col)
        collect it))

(defun col-defined-spots (array spot)
  "Return the numbers on the same column as SPOT in ARRAY that are not NIL."
  (loop for row to (array-dimension array 0)
        for col = (cdr spot)
        if (aref array row col)
        collect it))

(defun sqr-defined-spots (array spot)
  "Return the numbers on the same 3x3 square as SPOT in ARRAY that are not NIL."
  (do ((row (* 3 (floor (car spot) 3) (1+ row))) res)
      ((= row (* 3 (ceiling (car spot) 3))) res)
      (do ((col (* 3 (floor (cdr spot) 3)) (1+ col)))
          ((= col (* 3 (ceiling (cdr spot) 3))))
          (if (aref array row col)
              (push (aref array row col) res)))))
