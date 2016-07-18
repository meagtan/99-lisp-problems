;;;; Generating k-regular graphs with n nodes

;;; p94

(defun regular-graphs (n k)
  "Return all non-isomorphic k-regular graphs with n nodes."
  (remove-duplicates
    (make-graphs k (range 1 n)) ;p22
    :test #'isomorphic-p)) ;p85

(defun make-graphs (deg nodes &optional (nodes-left nodes) edges)
  "Generate all (possibly isomorphic) k-regular graphs with the given nodes."
  (if (null nodes-left)
      (list (make-graph nodes edges)) ;p80
      (mapcan (lambda (comb)
                ;; try to connect (car nodes-left) to every element of COMB
                ;; if any element of COMB has degree >= DEG or is already connected to (car nodes-left), return NIL
                ;; otherwise add the created edges to EDGES and call make-graphs
                (and (every (lambda (n) 
                              (< (degree-from-edges n edges) deg)) 
                       comb)
                     (every (lambda (e) 
                              (set-difference (list (car nodes-left))
                                (set-difference (nodes e) comb)))
                       edges)
                     (make-graph deg nodes (cdr nodes-left) 
                       (append (mapcar (lambda (n) (cons (car nodes-left) n)) comb) edges))))
        (combinations (cdr nodes-left) 
                      (- deg (degree-from-edges (car nodes-left) edges))))))

(defun combinations (set k)
  "Return all K-element subsets of SET."
  (cond ((= k 0) (list NIL))
        ((null set) NIL)
        (T (append (mapcar (lambda (comb) (cons (car set) comb))
                     (combinations (cdr set) (1- k)))
                   (combinations (cdr set) k)))))

(defun degree-from-edges (node edges)
  "Calculate the degree of NODE given the list of edges."
  (loop for e in edges
    if (member node (nodes e))
    sum 1))
