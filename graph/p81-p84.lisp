;;;; Graph paths, spanning trees

;;; p81

(defun paths (graph a b &optional visited)
  "Return all acyclic paths from A to B."
  (mappend (lambda (edge &aux (n (end-node edge)))
             (unless (member n visited)
               (if (eq n b)
                   (list edge)
                   (cons edge (paths graph n b (cons a visited))))))
           (edges graph a)))

;;; p82

(defun cycles (graph a)
  "Return all cycles starting and ending in A."
  (mappend (lambda (edge &aux (n (end-node edge)))
             (cons edge (paths graph n a)))
           (edges graph a)))

;;; p83

(defun s-trees (graph)
  "Construct all spanning trees of a graph via backtracking."
  )

(defun s-tree-p (graph tree)
  "Return T if TREE is a spanning tree of GRAPH."
  (and (subgraph-p tree graph)
       (tree-p tree) ;will this halt?
       (null (set-exclusive-or (graph-nodes tree) (graph-nodes graph)))))

(defun tree-p (graph)
  "Return T if GRAPH is a tree."
  (member graph (s-trees graph)))

(defun connected-p (graph)
  "Return T if GRAPH is connected."
  (not (null (s-trees graph))))
