;;;; Constructing binary trees

;;; In Lisp we represent the empty tree by 'nil' and the non-empty tree by the list (X L R), 
;;;   where X denotes the root node and L and R denote the left and right subtree, respectively. 
;;; The example tree depicted opposite is therefore represented by the following list: 
;;;   (a (b (d nil nil) (e nil nil)) (c nil (f (g nil nil) nil))) 
;;; Other examples are a binary tree that consists of a root node only:
;;;   (a nil nil) or an empty binary tree: nil.

;;; p54

(defun tree-p (tree)
  "Return T if the argument represents a valid binary tree."
  (or (null tree)
      (and (listp tree)
           (= (length tree) 2)
           (tree-p (second tree))
           (tree-p (third tree)))))

;;; p55

(defun cbal-trees (n)
  "Construct all possible completely balanced trees, i.e. trees for whose every node the difference of the number of nodes
for each subtree is at most 1, with N nodes in total, all containing the symbol X as its value."
  (when (> n 0)
    (mappend #'generate-trees (cbal-trees (1- n)))))

;; will this generate any duplicates?
(defun generate-trees (tree &optional (sym 'X))
  "Generate all completely balanced trees that can be reached by adding a node to the leaves of TREE."
  (if (null tree) 
      (list sym NIL NIL)
      (let ((n1 (nodes (second tree)))
            (n2 (nodes (third tree))))
        (cond ((= n1 n2)
               ;; add nodes to both branches
               (append (mapcar (lambda (tr) (list sym (second tree) tr))
                         (generate-trees (third tree)))
                       (mapcar (lambda (tr) (list sym tr (third tree)))
                         (generate-trees (second tree)))))
              ((< n1 n2)
               ;; only add to left
               (mapcar (lambda (tr) (list sym tr (third tree)))
                 (generate-trees (second tree))))
              (T
               ;; only add to right
               (mapcar (lambda (tr) (list sym (second tree) tr))
                 (generate-trees (third tree))))))))

;;; p56

(defun symmetric-p (tree)
  "Return T if the left branch of TREE is the mirror image of the right branch."
  (or (null tree)
      (mirror-p (second tree) (third tree))))

(defun mirror-p (tree1 tree2)
  "Return T if TREE1 is the mirror image of TREE2."
  (if (null tree1)
      (null tree2)
      (and (mirror-p (second tree1) (third tree2))
           (mirror-p (third tree1) (second tree2)))))

;;; p57

(defun make-bst (list)
  "Construct binary search tree from list of numbers."
  (do ((tree NIL (bst-insert (pop list) tree)))
      ((null list) tree)))

(defun bst-insert (item tree)
  "Insert item into binary search tree."
  (cond ((null tree) (list item NIL NIL))
        ((= item (first tree)) tree)
        ((< item (first tree))
         (list (first tree) 
               (bst-insert item (second tree)) 
               (third tree)))
        (T
         (list (first tree)
               (second tree)
               (bst-insert item (third tree))))))

(defun test-symmetric (list)
  "Test if the binary search tree constructed from LIST is symmetric."
  (symmetric-p (make-bst list)))
  
