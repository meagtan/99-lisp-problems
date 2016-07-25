;;;; Binary tree layout

(in-package #:99-lisp-problems)

;;; A layout assigns a pair of coordinates to each node,  so the node (item left right) is transformed into 
;;;   ((item (x . y)) left right).

;;; p64

(defun layout1 (tree &optional (level 1) (left-cousins 0))
  "Lay out binary tree so that the x-coordinate of each node represents its position on the inorder sequence 
and the y-coordinate its level."
  (when tree
    (list (list (first tree) (cons (+ 1 left-cousins (length (nodes (second tree)))) level))
          (layout1 (second tree) (1+ level) left-cousins)
          (layout1 (third tree) (1+ level) (+ 1 left-cousins (length (nodes (second tree))))))))

;;; p65

;; start-x can be eliminated by laying out the left branch first and adding distance to the x-coord of its root
(defun layout2 (tree &optional (level 1) (distance (expt 2 (- (height tree) 2))) (start-x (start-x tree)))
  "Lay out binary tree so that the horizontal distance between each neighbor at a given level is constant."
  (when tree
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
  (when tree
    ;; lay out both the left and the right branch first with their leftmost leaves at x coordinate 0, then shift accordingly
    (let* ((left (layout3 (second tree) (1+ level)))
           (right (layout3 (third tree) (1+ level)))
           (distance (min-distance left right)))
      (list (list (first tree) (cons (+ distance (x-coord left)) level))
            left
            (shift-right (- (+ (* 2 distance) (x-coord left)) (x-coord right) right)))))

(defun shift-right (dx tree)
  "Shift the x coordinate of each node of tree by dx."
  (when tree
    (list (list (first (first tree)) (cons (+ dx (x-coord tree)) (y-coord tree)))
          (shift-right dx (second tree))
          (shift-right dx (third tree)))))

(defun min-distance (left right)
  "Return the minimum distance between the roots of LEFT and RIGHT, laid out starting from the y axis, 
so that the trees don't intersect."
  (do ((distance (- (x-coord left) (x-coord (second left))) (1+ distance)))
      ((not (intersect-p left right (* 2 distance))) distance)))

(defun intersect-p (left right dx &aux (rdx (- (+ (x-coord left) dx)) (x-coord right)))
  "Return T if a node of LEFT intersects a node of RIGHT when the roots are separated by DX."
  (some (lambda (level)
          ;; if two points on different trees intersect
          (intersection (mapcar #'x-coord (nodes-at-level level left))
                        (mapcar (lambda (node) (+ rdx (x-coord node)))
                          (nodes-at-level level right))))
        (range 1 (1- (min (height left) (height right))))))

(defun rightmost (tree)
  "Return the x coordinate of the rightmost node in TREE."
  (first (sort (mapcar #'x-coord (inorder tree)) #'>)))
  
;;; Auxiliary position functions

(defun x-coord (tree)
  (and tree (tree-p tree)
    (car (second (first tree)))))
    
(defun y-coord (tree)
  (and tree (tree-p tree)
    (cdr (second (first tree)))))
