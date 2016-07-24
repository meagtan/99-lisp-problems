;;;; Crossword puzzle solver

;;; p99

(defun solve-puzzle (word-list puzzle)
  "Solve a crossword puzzle, inputted as a string, and return the solved puzzle as a string."
  (let ((graph (assign-words word-list (puzzle-graph puzzle))))
    (when graph
      (graph-puzzle graph))))

(defun assign-words (word-list graph)
  "Assign words in the given list to each site of GRAPH, in a way compatible with the intersections of each site."
  ;; go through each site, assign words of appropriate length, filter based on intersections
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
  ;; scan row by row
  )

(defun graph-puzzle (graph)
  "Convert the graph representation of a puzzle into a string."
  )
