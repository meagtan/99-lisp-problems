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
                ;; try to connect (car nodes) to every element of COMB
                ;; if any element of COMB has degree >= DEG, return NIL
                ;; otherwise add the created edges to EDGES and call make-graphs
                )
        (combinations (cdr nodes) deg))))

(defun combinations (set k)
  "Return all K-element subsets of SET."
  (cond ((= k 0) (list NIL))
        ((null set) NIL)
        (T (append (mapcar (lambda (comb) (cons (car set) comb))
                     (combinations (cdr set) (1- k)))
                   (combinations (cdr set) k)))))
