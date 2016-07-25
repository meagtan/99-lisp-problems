;;;; Sudoku solver

(in-package :99-lisp-problems)

;;; p97

(defun solve-sudoku (array &optional (spot (next-undefined-spot array)))
  "Return solutions to a valid Sudoku puzzle, represented by a 9x9 array with elements either an integer between 1 and 9 or NIL."
  (if (null spot)
      (list array)
      (mapcan (lambda (n &aux (a (copy-seq array))) ;expensive
                (setf (aref a (car spot) (cdr spot)) n)
                (solve-sudoku a (next-undefined-spot a spot)))
              (set-difference (range 1 9) ;p22
                (union (row-defined-spots array spot)
                       (col-defined-spots array spot)
                       (sqr-defined-spots array spot))))))

(defun solve-sudoku-iter (array &aux res (defined-num (defined-num array)))
  "Alternate iterative version of SOLVE-SUDOKU."
  (do* ((queue (list NIL)  (cdr queue))
        (binds (car queue) (car queue)) ;a list containing bindings for spots
        (spot (next-undefined-spot array (mapcar #'car binds))
              (next-undefined-spot array (mapcar #'car binds))))
       ((null queue) res)
       (if (= (length binds) defined-num)
           ;; add valid solution to list
           (push (apply-bindings binds array) res)
           (setf queue 
             (append queue
               (mapcar (lambda (n)
                         (cons (cons spot n) binds))
                 (set-difference (range 1 9)
                   (union (row-defined-spots array spot binds)
                          (col-defined-spots array spot binds)
                          (sqr-defined-spots array spot binds)))))))))

(defun apply-bindings (binds array)
  "Apply the given bindings to ARRAY."
  (setf array (copy-seq array))
  (loop for ((row . col) . n) in binds
    do (setf (aref array row col) n))
  array)

(defun next-undefined-spot (array &optional defined-spots (start (cons 0 0)))
  "Return the next spot of ARRAY, read from left to right and from top to bottom, that comes after START and is NIL."
  (do ((row (car start)) 
       (col (cdr start)))
      ((= row (array-dimension array 0)))
      (unless (or (aref array row col) 
                  (member (cons row col) defined-spots :test #'equal))
        (return (cons row col)))
      (if (= (incf col) (array-dimension array 1))
          (setf row (1+ row) col 0))))

(defun row-defined-spots (array spot &optional binds)
  "Return the numbers on the same row as SPOT in ARRAY that are not NIL."
  (loop for col to (array-dimension array 1)
        for row = (car spot)
        for bind = (assoc (cons row col) binds :test #'equal)
    if (aref array row col)
      collect it
    if bind
      collect (cdr bind)))

(defun col-defined-spots (array spot &optional binds)
  "Return the numbers on the same column as SPOT in ARRAY that are not NIL."
  (loop for row to (array-dimension array 0)
        for col = (cdr spot)
        for bind = (assoc (cons row col) binds :test #'equal)
    if (aref array row col)
      collect it
    if bind
      collect (cdr bind)))

(defun sqr-defined-spots (array spot &optional binds)
  "Return the numbers on the same 3x3 square as SPOT in ARRAY that are not NIL."
  (do ((row (* 3 (floor (car spot) 3) (1+ row))) res)
      ((= row (* 3 (ceiling (car spot) 3))) res)
      (do ((col (* 3 (floor (cdr spot) 3)) (1+ col)))
          ((= col (* 3 (ceiling (cdr spot) 3))))
          (if (aref array row col)
              (push (aref array row col) res))
          (if (assoc (cons row col) binds :test #'equal)
              (push (assoc (cons row col) binds :test #'equal) res)))))

(defun print-sudoku (array)
  "Print a 9x9 array representing a Sudoku puzzle."
  (dotimes (row 9)
    ;; horizontal separators
    (when (> row 0) 
      (if (= (rem row 3) 0)
          (print "--------+---------+--------")
          (print "        |         |        ")))
    ;; print each column
    (dotimes (col 9)
      ;; column separators
      (when (> col 0)
        (format t " ~[|~] " (rem col 3)))
      ;; print array[row, col] if not NIL, else print .
      (format t "~[.~;~:*~D~]" (aref array row col)))))

;;; p98

(defun solve-nonogram (rows cols &optional puzzle)
  "Solve the nonogram puzzle represented by the given lists of solid lengths across each row and column.
A full cell in a solution of the puzzle will be represented by T, and an empty cell by NIL."
  (if (null rows)
      (list puzzle)
      ;; start from the last row
      (mapcan (lambda (row &aux (p (partition-cols cols row)))
                ;; if every cell is compatible with the current state of columns
                (when (every #'identity (mapcar #'car p))
                  (solve-nonogram (butlast rows)
                                  (mapcar #'cdr p)
                                  (cons row puzzle))))
              (generate-rows (car (last rows)) (length cols)))))

(defun solve-nonogram-iter (rows cols &aux res)
  "Alternative iterative version of SOLVE-NONOGRAM."
  (do* ((queue (list (cons NIL cols)) (cdr queue))
        (step (car queue) (car queue))) ;a pair of filled rows and a column list
       ((null queue) res)
       (if (= (length (car step)) (length rows))
           (push (car step) res)
           (setf queue 
             (append queue
               (loop for row in (generate-rows (nth (- (length rows)
                                                       (length (car step)))
                                                    rows)
                                               (length cols))
                     for p = (partition-cols (cdr step) row)
                 if (every #'identity (mapcar #'car p))
                   collect (cons (cons row (car step))
                                 (mapcar #'cdr p))))))))

;; A column is represented as a list of numbers with last element possibly NIL, depending on if the last row can be filled.

(defun partition-cols (row cols)
  "Separate COLS into firsts and rests based on whether row is T at each column."
  (loop for cell in row
        for col in cols
    if cell 
      collect (cons (column-first col) (column-rest col))
    else
      collect (cons T col)))

(defun column-first (col)
  "Return T if a cell in the last row and same column as COL can be filled."
  (not (null (car (last col)))))

(defun column-rest (col)
  "Return the rest of COL after the last row has been applied."
  (case (car (last col))
    (NIL (butlast col)) ;works also if col is NIL
    (1   (append (butlast col) (list NIL)))
    (T   (append (butlast col) (mapcar #'1- (last col))))))

(defun generate-rows (row-list n)
  "Collect ways to fill a row with T and NIL based on the given list of lengths."
  ;; partition (- n (length row-list)) empty cells into (1+ (length row-list)) solid parts
  (mapcar (lambda (partition &aux (rows (row-list)))
            (append (make-list (car partition))
                    (loop for i to (1- (length row-list))
                          for fulls   = (pop rows)
                          for empties = (pop partition)
                      ;; make sure every intermediate empty block is full
                      if (or (= i (1- (length row-list)))
                             (> empties 0))
                      append 
                        (append (make-list fulls :initial-element T)
                                (make-list empties)))))
          (partitions (- n (length row-list))
                      (1+ (length row-list)))))

(defun partitions (n k)
  "Partition N into K natural numbers, dependent of ordering."
  (and (>= n 0) (> k 0)
    (do* ((queue (list NIL) (cdr queue))
          (part (car queue) (car queue)) ;an incomplete partition
          res)
         ((null queue) res)
         (if (= (length part) k)
             (when (= (reduce #'+ part) n) 
               (push part res))
             (setf queue
               (append queue
                 (mapcar (lambda (d)
                           (cons d part))
                         (range 0 (- n (reduce #'+ part)))))))))) ;p22
