;;;; Graph paths, spanning trees

(in-package #:99-lisp-problems)

;;; p81

(defun paths (graph a b)
  "Return all acyclic paths from A to B."
  (do ((stack (list (list a)) (cdr stack)) ;each element of the stack is an incomplete path in reverse order
       res)
      ((null stack) res)
      (dolist (edge (edges (caar stack) graph))
        (let ((n (other-node edge (caar stack))))
          (unless (member n (cdar stack))
            (if (equal n b) ;end of path
                (push (reverse (car stack)) res)
                (push (cons n (car stack)) (cdr stack))))))))

;;; p82

(defun cycles (graph a)
  "Return all cycles starting and ending in A."
  (mapcan (lambda (edge &aux (n (end-node edge)))
            (cons edge (remove (list edge) (paths graph n a) :test #'equal)))
          (edges a graph)))

;;; p83

;; can this be simplified? what can one say about the different trees calculated?
(defun s-trees (graph &aux (len (length (graph-nodes graph))))
  "Generate all spanning trees of GRAPH, where trees are represented as lists of edges."
  (do ((stack (list (cons NIL NIL))) ;each element of the stack contains list of visited edges and nodes
       res)
      ((null stack) res)
      (let* ((elem (car stack)) 
             (edges (car elem))
             (nodes (cdr elem)))
        (if (= (length nodes) len) ;complete tree
            (pushnew edges res :test-not #'set-exclusive-or)
            (dolist (edge (set-difference (graph-edges graph) edges)) ;perhaps include this in elem
              (unless (subsetp (nodes edge) nodes) ;the inclusion of edge doesn't form a cycle
                (push (cons (cons edge edges) (union (nodes edge) nodes)) (cdr stack))))))))

(defun s-tree-p (graph tree)
  "Return T if TREE is a spanning tree of GRAPH."
  (member tree (s-trees graph)))

(defun graph-tree-p (graph)
  "Return T if GRAPH is a tree."
  (s-tree-p graph graph))

(defun connected-p (graph)
  "Return T if GRAPH is connected."
  (not (null (s-trees graph))))

;;; p84

(defun ms-tree (graph)
  "Return minimal spanning tree of GRAPH."
  (do ((nodes (graph-nodes graph))
       (costs (mapcar #'list (graph-nodes graph))) ;map nodes to least cost
       (edges (mapcar #'list (graph-nodes graph))) ;map nodes to least costly edge
       (res (make-graph))
       node)
      ((null nodes) res)
      (setf node (pred-min nodes #'cost< :key (lambda (n) (assoc n costs)))) ;node with lowest cost
      (setf nodes (remove node nodes :test #'equal))
      (push node (graph-nodes res))
      (when (cdr (assoc node edges))
        (push (cdr (assoc node edges)) (graph-edges res)))
      (dolist (edge (edges node graph))
        (let ((other (other-node edge node)))
          (and (member other nodes)
               (cost< (edge-weight edge) (cdr (assoc other costs)))
               (rplacd (assoc other costs) (edge-weight edge))
               (rplacd (assoc other edges) edge))))))

(defun cost< (cost1 cost2)
  "Order costs such that NIL corresponds to positive infinity."
  (cond ((null cost1) NIL) ;also includes cost2 being NIL
        ((null cost2) T)
        (T (< cost1 cost2))))

(defun pred-min (list pred &key (key identity))
  "Return minimum of list based on predicate, such that (pred-min list #'<) is (apply #'min list)."
  (do ((list (cdr list) (cdr list))
       (min (car list)))
      ((null list) min)
      (if (funcall pred (funcall key (car list))
                        (funcall key min))
          (setf min (car list)))))
