;;;; Von Koch's conjecture

(in-package :99-lisp-problems)

;;; p92

(defun von-koch (tree &aux (len (length (graph-nodes tree))))
  "Given a tree with N nodes (hence N - 1 edges), return enumerations (as an alist for each node) of the nodes and 
edges of the tree, such that for each edge numbered K, the difference between the node numbers of its nodes equals K."
  (when (tree-p tree)
    (if (= len 1)
        (list (list (cons (car (graph-nodes tree)) 1)))
        (bind-edges (graph-edges tree) (range 1 len) (range 1 (1- len))))))

(defun bind-edges (edges node-list edge-list &optional acc)
  "Bind each edge in EDGES to a number in EDGE-LIST, so that it is the difference between the numbering of its two nodes."
  (if (null edges)
      acc
      (let ((defined (remove-if-not (lambda (n) (assoc n acc)) (nodes (car edges)))))
        (case (length defined)
          ;; both nodes are defined, check if (car edges) can be associated to a number in edge-list
          (2 (when (member (abs (- (assoc (start-node (car edges)) acc)
                                   (assoc (end-node (car edges)) acc)))
                           edge-list)
                (bind-edges (cdr edges) node-list 
                  (remove (abs (- (assoc (start-node (car edges)) acc)
                                  (assoc (end-node (car edges)) acc)))
                          edge-list)
                  acc)))
          ;; one node is defined, try each possibility for a numbering of (car edges)
          (1 (mapcan (lambda (edge-num &aux (n1 (assoc (car defined) acc)))
                       (mapcar (lambda (n2)
                                 (bind-edges (cdr edges) 
                                             (remove n2 node-list) 
                                             (remove edge-num edge-list) 
                                             (cons (cons (other-node (car edges) (car defined)) n2) acc)))
                               (intersection (list (+ n1 edge-num) (- n1 edge-num)) node-list)))
                     edge-list))
          ;; no nodes are defined, try each possibility for a numbering of (car edges)
          (T (mapcan (lambda (edge-num)
                       (mapcar (lambda (ns &aux (n1 (car ns)) (n2 (cdr ns)))
                                 (bind-edges (cdr edges) 
                                             (set-difference node-list (list n1 n2)) 
                                             (remove edge-num edge-list) 
                                             (cons (cons (start-node (car edges)) n1)
                                               (cons (cons (end-node (car edges)) n2) acc))))
                               (node-ns edge-num node-list)))
                     edge-list))))))

(defun node-ns (edge-num node-list)
  "Return the pairs of node numbers in NODE-LIST whose difference is equal to EDGE-NUM."
  (mapcan (lambda (n1)
            (mapcar (lambda (n2) (cons n1 n2))
              (intersection (list (+ n1 edge-num) (- n1 edge-num)) node-list)))
          node-list))
