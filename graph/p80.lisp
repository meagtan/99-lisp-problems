;;;; Graph formats, conversions

;;; p80

;;; We shall consider three formats each for graphs, digraphs and weighted graphs:
;;; - (default) Graph-term form, containing sets of nodes and edges.
;;; - Adjacency list form, an association list mapping each node to its neighboring nodes.
;;; - Human readable form, containing either edges or sole nodes for nodes not connected to any other node.

;;; An edge is either a cons pair of nodes, in the case of unweighted edges, or a list of nodes and a weight.

(defstruct graph 
  "A graph containing nodes and edges, which might be weighted or unweighted."
  nodes edges directed)

(defun make-ud-graph (nodes edges)
  "Make undirected graph with given nodes and edges."
  (make-graph :nodes nodes :edges edges :directed NIL))
  
(defun make-d-graph (nodes edges)
  "Make directed graph with given nodes and edges."
  (make-graph :nodes nodes :edges edges :directed T))

(defun adjacency-list (graph)
  "Return the adjacency list of a graph."
  (mapcar (lambda (node)
            (cons node 
                  (mapcar #'cdr 
                    (remove-if-not (lambda (edge) (edge-of edge node graph)) 
                                   (graph-edges graph)))))
          (graph-nodes graph)))

(defun readable-list (graph)
  "Return a list of edges and lone nodes of a graph."
  (append (graph-edges graph)
          (lone-nodes graph)))

(defun lone-nodes (graph)
  "Return a list of nodes of a graph not connected to any edge."
  (set-difference (graph-nodes graph)
    (mapcan #'nodes (graph-edges graph))))

(defun ud-adjacency-graph (adj-list &aux (nodes (mapcar #'car adj-list)))
  "Make undirected graph from the given adjacency list."
  (make-ud-graph
    nodes
    (let (edges)
      (mapc (lambda (n)
        (mapc (lambda (x &aux (e (cons n x)))
          (unless (member e edges :test (lambda (l1 l2) (null (set-exclusive-or l1 l2)))
                                  :key #'nodes)
            (push e edges)))
          (cdr (assoc n adj-list))))
        nodes)
      edges)))

(defun d-adjacency-graph (adj-list &aux (nodes (mapcar #'car adj-list)))
  "Make directed graph from the given adjacency list."
  (make-d-graph
    nodes
    (let (edges)
      (mapc (lambda (n)
        (mapc (lambda (x &aux (e (cons n x)))
                (push e edges))
          (cdr (assoc n adj-list))))
        nodes)
      edges)))

(defun ud-readable-graph (list &aux (lone-nodes (remove-if-not #'atom list)) (edges (remove-if-not #'edge-p list)))
  "Make undirected graph from the given readable list of edges and lone nodes."
  (make-ud-graph (append lone-nodes
                         (mapcan #'nodes edges))
                 edges))

(defun d-readable-graph (list &aux (lone-nodes (remove-if-not #'atom list)) (edges (remove-if-not #'edge-p list)))
  "Make directed graph from the given readable list of edges and lone nodes."
  (make-d-graph (append lone-nodes
                        (mapcan #'nodes edges))
                edges))

(defun edge-of (edge node graph)
  "Return T if edge starts from node."
  (or (eq node (car edge))
      (if (not (graph-directed graph)) ;if undirected, check end
          (eq node (end-node edge)))))

(defun nodes (edge)
  "Return the nodes of an edge."
  (list (car edge)
        (end-node edge)))

(defun edge-p (edge)
  "Return T if the given expression is a valid edge."
  (and (consp edge)
       (or (atom (cdr edge))
           (atom (cadr edge))
           (null (cdddr edge)))))

(defun edge (graph node1 node2)
  "If there is an edge from NODE1 to NODE2, return T (or the weight of the edge if the graph is weighted)."
  (let ((edges (assoc node1 (adjacency-list graph))))
    (if edges
      (let ((edge-end (find node2 (cdr edges) 
                        :test (lambda (n e)
                                (or (eq n e)
                                    (if (consp e)
                                      (eq n (car e))))))))
        (or (atom edge-end)
            (cadr edge-end))))))

(defun edges (node graph)
  "Return all edges in graph that start from node."
  (mapcar (lambda (x) (cons node x)) (cdr (assoc node (adjacency-list graph)))))

(defun neighbors (node graph)
  "Return all neighbors of node in graph."
  (mapcar (lambda (end)
            (if (atom end)
                end
                (car end)))
          (cdr (assoc node (adjacency-list graph)))))

(defun start-node (edge)
  "Return the starting node of an edge."
  (car edge))

(defun end-node (edge)
  "Return the ending node of an edge."
  (if (consp (cdr edge))
      (cadr edge)
      (cdr edge)))

(defun empty-p (graph)
  (null (graph-nodes graph)))

(defun subgraph-p (graph1 graph2)
  "Return T if GRAPH1 is a subgraph of GRAPH2."
  (and (graph-p graph1)
       (graph-p graph2)
       (subsetp (graph-nodes graph1) (graph-nodes graph2))
       (subsetp (graph-edges graph1) (graph-edges graph2))))

(defun graph-equal-p (graph1 graph2)
  "Return T if GRAPH1 and GRAPH2 are the same graph."
  (null (set-exclusive-or (adjacency-list graph1) (adjacency-list graph2))))
