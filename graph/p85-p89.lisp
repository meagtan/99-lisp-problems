;;;; Graph isomorphism, traversal, etc.

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

;; p86

(defun degree (node graph)
  "Return the degree of node in graph."
  (length (cdr (assoc node (adjacency-list graph)))))

(defun sorted-nodes (graph)
  "Return list of nodes of graph, sorted according to decreasing degree."
  (sort (graph-nodes graph) #'> :key (lambda (node) (degree node graph))))

(defun color (graph &aux (res (mapcar #'list (graph-nodes graph))) (counter 0))
  "Return alist representing a proper coloring of the vertices of the given graph, colors represented as a positive integer."
  (labels ((new-color () (incf counter)))
    ;; Welsh-Powell algorithm
    (do ((nodes (sorted-nodes graph))
         (node (car nodes) (car nodes))
         (color (new-color) (new-color)))
        ((null nodes) res)
        ;; color all nodes not connected to node, including node itself, with color
        ;; and remove them from the list of sorted nodes
        (delete-if (lambda (n)
                     (and (not (or (edge node n graph) (edge n node graph)))
                          (rplacd (assoc n res) color)))
          nodes))))
