;;;; Von Koch's conjecture

;;; p92

(defun von-koch (tree)
  "Given a tree with N nodes (hence N - 1 edges), return an enumeration of the nodes and edges of the tree,
such that for each edge numbered K, the difference between the node numbers of its nodes equals K."
  ;; The edge numbered N - 1 must connect nodes 1 and N.
  ;; For each node, try that node as node 1. 
  ;; If it is connected only to 1 node, remove it from the tree, number the rest and increment each of the node numbers.
  ;; Else, try to remove an edge.
  )
