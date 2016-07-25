;;;; Graph paths, spanning trees

;;; p81

(defun paths (graph a b &optional visited)
  "Return all acyclic paths from A to B."
  (mapcan (lambda (edge &aux (n (end-node edge)))
            (unless (member n visited)
              (if (eq n b)
                  (list edge)
                  (cons edge (paths graph n b (cons a visited))))))
          (edges a graph)))

;;; p82

(defun cycles (graph a)
  "Return all cycles starting and ending in A."
  (mapcan (lambda (edge &aux (n (end-node edge)))
            (cons edge (remove (list edge) (paths graph n a) :test #'equal)))
          (edges a graph)))

;;; p83

;; can this be simplified? what can one say about the different trees calculated?
(defun s-trees (graph &optional visited-ns visited-es)
  "Generate all spanning trees of GRAPH, where trees are represented as lists of edges."
  (munion (lambda (node)
             (munion (lambda (edge) 
                        (unless (subsetp (nodes edge) visited-ns)
                          (mapcar (lambda (tree) (cons edge tree))
                            (s-trees graph (cons node visited-ns)
                                           (cons edge visited-es)))))
               (set-difference (edges node graph) visited-es)))
          (set-difference (graph-nodes graph) visited-ns)))

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

;;; p84

(defun ms-trees (graph &optional visited-ns visited-es)
  "Return minimal spanning trees of GRAPH."
  (munion (lambda (node 
                   &aux (edges (remove-if (lambda (edge)
                                            (subsetp (nodes edge) visited-ns))
                                  (sort #'edge< (set-difference (edges node graph) visited-es)))))
             ;; only pick the edge with the least weight
             (when edges
                (mapcar (lambda (tree) (cons (first edges) tree))
                  (ms-trees graph (cons node visited-ns)
                                  (cons (first edges) visited-es)))))
          (set-difference (graph-nodes graph) visited-ns)))

(defun edge< (edge1 edge2)
  "Return T if EDGE1 has less weight, if any, than EDGE2."
  (or (atom (cdr edge1))
      (and (consp (cdr edge2))
           (< (third edge1) (third edge2)))))
