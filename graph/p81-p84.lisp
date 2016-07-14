;;;; Graph paths, spanning trees

;;; p81

(defun paths (graph a b &optional visited)
  "Return all acyclic paths from A to B."
  (mapcan (lambda (edge &aux (n (end-node edge)))
            (unless (member n visited)
              (if (eq n b)
                  (list edge)
                  (cons edge (paths graph n b (cons a visited))))))
          (edges graph a)))

;;; p82

(defun cycles (graph a)
  "Return all cycles starting and ending in A."
  (mapcan (lambda (edge &aux (n (end-node edge)))
            (cons edge (paths graph n a)))
          (edges graph a)))

;;; p83

(defun s-trees (graph &optional visited-ns visited-es)
  "Generate all spanning trees of GRAPH, where trees are represented as lists of edges."
  (munion (lambda (node)
             (munion (lambda (edge) 
                        (unless (subsetp (nodes edge) visited-ns)
                          (munion (lambda (tree) (cons edge tree))
                            (s-trees graph (cons node visited-ns)
                                           (cons edge visited-es)))))
               (set-difference (edges graph node) visited-es)))
           (graph-nodes graph)))

(defun s-tree-p (graph tree)
  "Return T if TREE is a spanning tree of GRAPH."
  (member tree (s-trees graph)))

(defun tree-p (graph)
  "Return T if GRAPH is a tree."
  (s-tree-p graph graph))

(defun connected-p (graph)
  "Return T if GRAPH is connected."
  (not (null (s-trees graph))))

(defun munion (function list &rest lists)
  "Return the union of the application of FUNCTION to successive elements of LIST."
  (reduce #'union (apply #'mapcar function list lists)))
