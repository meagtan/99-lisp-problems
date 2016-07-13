;;;; Binary tree layout

;;; A layout assigns a pair of coordinates to each node,  so the node (item left right) is transformed into 
;;;   ((item (x . y)) left right).

;;; p64

(defun layout1 (tree &optional (level 1) (left-cousins 0))
  "Lay out binary tree so that the x-coordinate of each node represents its position on the inorder sequence 
and the y-coordinate its level."
  (and tree (tree-p tree)
    (list (list (first tree) (cons (+ 1 left-cousins (length (nodes (second tree)))) level))
          (layout1 (second tree) (1+ level) left-cousins)
          (layout1 (third tree) (1+ level) (+ 1 left-cousins (length (nodes (second tree))))))))

;;; p65

;; start-x can be eliminated by laying out the left branch first and adding distance to the x-coord of its root
(defun layout2 (tree &optional (level 1) (distance (expt 2 (- (height tree) 2))) (start-x (start-x tree)))
  "Lay out binary tree so that the horizontal distance between each neighbor at a given level is constant."
  (and tree (tree-p tree)
    (list (list (first tree) (cons start-x level))
          (layout2 (second tree) (1+ level) (/ distance 2) (- start-x distance))
          (layout2 (third  tree) (1+ level) (/ distance 2) (+ start-x distance)))))

(defun start-x (tree &aux (distance (expt 2 (- (height tree) 2))))
  "The x-coordinate of the root node of tree in layout2."
  (if (second tree)
      (+ distance (start-x (second tree)))
      0))

;;; p66

(defun layout3 (tree &optional (level 1))
  "Lay out binary tree so that the horizontal distance between each neighbor at a given level is constant and as small as
possible without two nodes occupying the same position."
  (and tree (tree-p tree)
    ;; first lay out the two branches, then set distance so that the left and right branches don't cross
    ;; lay out both the left and the right branch first with their leftmost leaves at x coordinate 0, then shift accordingly
    ))
