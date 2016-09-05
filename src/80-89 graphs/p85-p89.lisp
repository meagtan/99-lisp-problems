;;;; Graph isomorphism, traversal, etc.

(in-package #:99-lisp-problems)

;;; p85

(defun isomorphic-p (graph1 graph2)
  "Determine whether the two graphs are isomorphic, and if so, return an isomorphism in the form of an alist."
  (and (= (length (graph-nodes graph1))
          (length (graph-nodes graph2)))
       (= (length (graph-edges graph1))
          (length (graph-edges graph2)))
       (= (length (lone-nodes  graph1))
          (length (lone-nodes  graph2)))
       (append (mapcar #'cons (lone-nodes graph1) (lone-nodes graph2))
               (car (find-isomorphisms graph1 graph2)))))

(defun find-isomorphisms (graph1 graph2 &optional (edges1 (graph-edges graph1)) (edges2 (graph-edges graph2)) res &aux found)
  "Detect isomorphisms between two graphs with the given edges."
  (assert (= (length edges1)) (= (length edges2)))
  (cond ((null edges1)
         res)
        ((setf found (find-matches (car edges1) edges2 graph1 graph2 res))
         (mapcan (lambda (match) 
                   (find-isomorphisms graph1 graph2 (cdr edges1) 
                                      (remove (car match) edges2) 
                                      (append (cdr match) res)))
                 found))))

(defun find-matches (edge edges graph1 graph2 map &aux (start (start-node edge)) (end (end-node edge)) node)
  "Return list of possible matches of edge to an element of edges that are compatible with the established vertex mapping."
  (cond ((setf node (assoc start map))
         (match-node end node graph1 graph2))
        ((setf node (assoc end map))
         (match-node start node graph1 graph2))
        (T 
         ;; try mapping edge to each edge in edges whose nodes aren't mapped to any node
         (mapcan (lambda (e &aux (start2 (start-node e)) (end2 (end-node e)) matches)
                   ;; match start-node to start-node or to end-node
                   (and (= (degree start graph1)
                           (degree start2 graph2))
                        (= (degree end graph1)
                           (degree end2 graph2))
                        (push (list e (cons start start2) (cons end end2)) matches))
                   (and (= (degree start graph1)
                           (degree end2 graph2))
                        (= (degree end graph1)
                           (degree start2 graph2))
                        (push (list e (cons start end2) (cons end start2)) matches))
                   matches)
            (remove-if (lambda (e) (intersection (mapcar #'cdr map) (nodes e))) edges)))))

(defun match-node (end start graph1 graph2)
  "Return list of matches of edge in GRAPH1 ending in END to an edge in GRAPH2 starting with START."
  (mapcar (lambda (e)
            (list e (cons end (remove start (nodes e)))))
    (remove-if-not (lambda (e)
                     (and (edge-of e start graph2)
                          (= (degree end graph1)
                             (degree (remove start (nodes e)) graph2))))
                   edges)))

;;; p86

(defun degree (node graph)
  "Return the degree of node in graph."
  (if (graph-directed graph)
      (out-degree node graph)
      (+ (out-degree node graph)
         (in-degree  node graph))))

(defun out-degree (node graph)
  "Return the out-degree of node in graph."
  (length (cdr (assoc node (adjacency-list graph)))))

(defun in-degree (node graph)
  "Return the in-degree of node in graph."
  (loop for e in (graph-edges graph)
        if (eq node (end-node e))
        sum 1))

(defun sorted-nodes (graph)
  "Return list of nodes of graph, sorted according to decreasing degree."
  (sort (graph-nodes graph) #'> :key (lambda (node) (degree node graph))))

(defun color (graph &aux (res (mapcar #'list (graph-nodes graph))) (counter 0))
  "Return alist representing a proper coloring of the nodes of the given graph, colors represented as a positive integer."
  (labels ((new-color () (incf counter)))
    ;; Welsh-Powell algorithm
    (do ((nodes (sorted-nodes graph))
         (node (car nodes) (car nodes))
         (color (new-color) (new-color)))
        ((null nodes) res)
        ;; color all nodes not connected to node, including node itself, with color
        ;; and remove them from the list of sorted nodes
        (setf nodes
          (remove-if (lambda (n)
                       (and (not (or (edge node n graph) (edge n node graph)))
                            (rplacd (assoc n res) color)))
            nodes))))

;;; p87

(defun depth-traverse (graph start)
  "Traverse GRAPH depth-first, starting from START."
  (do* ((stack (list start) (cdr stack))
        (res stack))
       ((null stack) res)
       (dolist (n (neighbors (car stack) graph))
         ;; if not already visited, add n to stack (and thereby res)
         (unless (member n res)
           (push n (cdr stack))))))

;; returns spanning tree of connected component of start
(defun depth-traverse-edges (graph start)
  "Return list of unweighted edges traversed while going through GRAPH depth-first, starting from START."
  (do* ((stack (list start) (cdr stack))
        (visited stack)
        res
        (node (car stack) (car stack)))
       ((null stack) (nreverse res))
       (dolist (n (neighbors node graph))
         (unless (member n visited)
           (push res (cons node n))
           (push n (cdr stack))))))

;;; p88

(defun connected-components (graph)
  "Return list of connected components of GRAPH."
  (do ((nodes (sorted-nodes graph)) ;reach nodes with highest degree first
       res)
      ((null nodes) res)
      (dolist (n (depth-traverse graph (car nodes)))
         (setf nodes (remove n nodes))
         (push n res))))

;;; p89

(defun bipartite-p (graph &aux (colors (mapcar #'list (graph-nodes graph)))) ;initialize for assignment
  "If GRAPH is bipartite, return its two parts in a dotted pair; else return NIL."
  (labels ((next (color) (- 3 color)))
    (dolist (comp (connected-components graph))
      ;; try to color nodes in comp alternating between 1 and 2
      (rplacd (assoc (car comp) colors) 1)
      (dolist (e (depth-traverse-edges graph (car comp)))
        (assert (and (cdr (assoc (start-node e) colors))       ;since comp is traversed depth-first
                     (not (cdr (assoc (end-node e) colors))))) ;since e is drawn from a tree, (cdr e) must not be already visited
        (rplacd (assoc (end-node e) colors)
          (next (cdr (assoc (start-node e) colors))))
        ;; look through edges around e
        (dolist (edge (edges (end-node e) graph))
          ;; if there is a coloring conflict, return NIL
          (if (eq (assoc (start-node edge) colors)
                  (assoc (end-node   edge) colors))
              (return-from 'BIPARTITE-P))))))
  ;; partition nodes based on their coloring
  (let ((res (list NIL)))
    (dolist (coloring colors res)
      (case (cdr coloring)
        (1 (push (car coloring) (car res)))
        (2 (push (car coloring) (cdr res)))
        (T (return-from 'BIPARTITE-P)))))) ;if some node is left uncolored
