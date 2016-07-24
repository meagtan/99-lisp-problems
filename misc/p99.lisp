;;;; Crossword puzzle solver

;;; p99

;; The framework of a crossword puzzle is represented internally as a graph, with nodes representing sites, storing 
;;   the coordinates of the starting point, direction and length and (if defined) the contents of the site, and edges 
;;   representing the points at which sites intersect.
;; A site is a list of 4 elements: its starting coordinates, direction (either (cons 1 0) or (cons 0 1)), length and word.
;; An edge has as its weight a dotted pair representing its coordinates.

;; An example string representation of a puzzle is as follows:
;;   ......  .
;;   . .  .  .
;;   . ..... .
;;   . . . ...
;;     . ... .
;;    ...

(defun solve-puzzle (word-list puzzle &aux (graph (puzzle-graph puzzle)))
  "Solve a crossword puzzle, inputted as a string, and return the solved puzzle as a string."
  (when graph
    (assign-words word-list graph)
    (when graph
      (graph-puzzle graph))))

(defun assign-words (word-list graph)
  "Assign words in the given list to each site of GRAPH, in a way compatible with the intersections of each site."
  ;; go through each site, assign words of appropriate length, filter based on intersections
  (when (<= (length (graph-nodes graph))
            (length word-list))
    (do* ((stack (list NIL) (cdr stack)) ;stores alists binding sites to words
          found)
         ((or found (null stack))
          (when found
            (loop for (site word) in found
              do (setf (fourth site) word)
              finally (return T))))
         ;; if (car stack) has a binding for each site
         (if (= (length (car stack))
                (length (graph-nodes graph)))
             (setf found (car stack))
             (setf stack
               (append 
                 (loop with site = (car (set-difference (graph-nodes graph)
                                                        (mapcar #'car (car stack))
                                                        :test #'equal))
                       for word in (set-difference word-list
                                                   (mapcar #'cadr (car stack))
                                                   :test #'equal)
                       if (and (= (third site) (length word))
                               ;; check for intersections
                               (every (lambda (edge &aux (other (other-node edge site)))
                                        (or (null (assoc other (car stack) :test #'equal))
                                            (char= (char word (distance edge site))
                                                   (char (second (assoc other (car stack) :test #'equal))
                                                         (distance edge other)))))
                                      (edges site graph)))
                       collect (cons (list site word) (car stack)))
                 stack))))))

(defun distance (edge site)
  "Return the index of the intersection represented by EDGE on SITE."
  )

(defun puzzle-graph (puzzle)
  "Convert the string representation of a puzzle into a graph."
  ;; scan row by row
  )

(defun graph-puzzle (graph)
  "Convert the graph representation of a puzzle into a string."
  )
