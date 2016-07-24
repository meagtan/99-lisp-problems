;;;; Crossword puzzle solver

;;; p99

(defun solve-puzzle (word-list puzzle)
  "Solve a crossword puzzle, inputted as a string, and return the solved puzzle as a string."
  )

;; The framework of a crossword puzzle is represented internally as a graph, with nodes representing sites, storing 
;;   the coordinates of the starting point, direction and length and (if defined) the contents of the site, and edges 
;;   representing the points at which sites intersect.

;; An example string representation of a puzzle is as follows:
;;   ......  .
;;   . .  .  .
;;   . ..... .
;;   . . . ...
;;     . ... .
;;    ...

(defun puzzle-graph (puzzle)
  "Convert the string representation of a puzzle into a graph."
  )

(defun graph-puzzle (graph)
  "Convert the graph representation of a puzzle into a string."
  )
