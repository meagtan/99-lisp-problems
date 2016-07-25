;;;; Multiway tree operations

(in-package #:99-lisp-problems)

;;; We shall represent a multiway tree as a cons pair containing an atom and a list of subtrees, i.e. as (<atom> <subtree>*).
;;; Then binary trees become a subclass of multiway trees with two subtrees at each node.
;;; However, leaves will have no subtrees, not NIL subtrees. We need not then consider NIL trees.

;;; p70

(defun make-tree (str)
  "Construct tree from node string given by the syntax <node-string> ::= <letter> <node-string>* '^'."
  ;; traverse string backwards
  (do ((i (1- (length str)) (1- i))
       (stack NIL))
      ((< i 0) (pop stack))
      (case (char str i)
        (#\^ 
          ;; pop until you get a letter
          (do* ((item (pop stack) (pop stack))
                (list (cons item NIL) (cons item list)))
               ((atom item) (push list stack))))
        (T
          ;; add character to stack
          (push (intern (make-string 1 :initial-element (char str i)))
                stack)))))

(defun tree-string (tree)
  "Convert tree into node string given by the syntax <node-string> ::= <letter> <node-string>* '^'."
  (when (tree-p tree)
    (format nil "~a~{~a~^~}^" (car tree) (mapcar #'tree-string (cdr tree)))))

(defun tree-p (tree)
  "Return T if the given s-expression is a valid tree."
  (and tree
       (listp tree)
       (atom (car tree))
       (every #'tree-p (cdr tree)))))

(defun nnodes (tree)
  "Count the nodes of a multiway tree."
  (when (tree-p tree)
    (1+ (reduce #'+ (mapcar #'nnodes (cdr tree))))))

;;; p71

(defun ipl (tree)
  "Return the internal path length of a tree. The internal path length of a multiway tree 
is the total sum of the path lengths from the root to all nodes of the tree."
  (when (tree-p tree)
    ;; increment every ipl of subtrees, then add the subtrees themselves
    (reduce #'+ (mapcar (lambda (tr) (+ 2 (ipl tr))) (cdr tree)))))

;;; p72

(defun bottom-up (tree)
  "Sequence the nodes of the given tree bottom up."
  (when tree
    (nconc (mapcan #'bottom-up (cdr tree)) (list (car tree)))))

;;; p73

;;; The original problem concerned converting Prolog trees into the lispy format we have been using, so instead
;;;   we will convert Lisp trees into Prolog trees.
;;; The syntax of a Prolog tree is as follows:
;;;   <tree> ::= t(<letter>,[{<tree>{,<tree>}*}])

(defun prolog-tree (tree)
  "Convert tree into the Prolog tree format, with syntax <tree> ::= t(<letter>,[{<tree>{,<tree>}*}])."
  (format nil "t(~a,[~{~a~^,~}])" (car tree) (mapcar #'prolog-tree (cdr tree))))
