;;;; Collecting leaves, nodes, etc.

(in-package :99-lisp-problems)

;;; p61

(defun leaves (tree)
  "Collect the leaves, i.e. nodes with no successors, of the given tree."
  (and tree (tree-p tree) ;only collect for non-null trees
    (if (or (second tree) (third tree)) ;if not a leaf
        (append (leaves (second tree))
                (leaves (third tree)))
        (list (first tree)))))

(defun count-leaves (tree)
  "Count the leaves of the given tree."
  (length (leaves tree)))

;;; p62

(defun internals (tree)
  "Collect the internal nodes, nodes with nonempty successors, of the given tree."
  (and tree (tree-p tree)
       (or (second tree) (third tree)) ;if it contains successors
       (cons (first tree)
         (append (internals (second tree))
                 (internals (third tree))))))

;;; p62b

(defun nodes-at-level (level tree)
  "Collect the nodes at a given level in the tree.
A node of a binary tree is at level N if the path from the root to the node has length N-1. The root node is at level 1."
  (if (= level 1)
      (list tree)
      (append (nodes-at-level (1- level) (second tree))
              (nodes-at-level (1- level) (third tree)))))

(defun level-order (tree)
  "Collect the nodes of TREE ordered by level, using NODES-AT-LEVEL."
  (mapcan (lambda (level) (nodes-at-level level tree))
    (range 1 (height tree))))

(defun level-order-direct (tree)
  "Collect the nodes of TREE ordered by level, using breadth-first search."
  (do ((queue (list tree) (cdr queue)) 
       (acc NIL (cons (caar queue) acc))) ;add item in node
      ((null queue) acc)
      (nconc queue (remove-if #'null (cdar queue))))) ;only add non-null subtrees to queue

;;; p63

(defun complete-binary-tree (n &optional (address 1) &aux (sym 'X))
  "Construct a complete binary tree. A complete binary tree with height H is defined as follows: 
The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e. 2**(i-1) at the level i, 
note that we start counting the levels from 1 at the root).

We can assign an address number to each node in a complete binary tree by enumerating the nodes in level order, 
starting at the root with number 1. In doing so, we realize that for every node X with address A the following property holds:
The address of X's left and right successors are 2*A and 2*A+1, respectively, supposed the successors do exist."
  (when (<= address n)
      (list sym 
            (complete-binary-tree n (* 2 address))
            (complete-binary-tree n (1+ (* 2 address))))))

(defun complete-binary-tree-p (tree)
  "Return T if the given tree is a complete binary tree."
  (and (tree-p tree)
       ;; every level contains 2^(level - 1) nodes
       (every (lambda (level) (= (length (nodes-at-level level tree)) (expt 2 (1- level))))
         (range 1 (1- (height tree))))))
