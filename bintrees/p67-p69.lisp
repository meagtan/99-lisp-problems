;;;; Representations of binary trees

;;; p67

(defun tree-to-string (tree)
  "Convert the given tree into a string, such that NIL is mapped to \"\", (item NIL NIL) to item,
and (item left right) to item(left,right)."
  (if tree
      (if (or (second tree) (third tree))
          (format nil "~a(~a,~a)" (first tree) 
                                  (tree-to-string (second tree)) 
                                  (tree-to-string (third tree)))
          (first tree))
      ""))

;; not the most efficient parser, but will do
(defun string-to-tree (str)
  "Convert the given string into a tree, such that \"\" is mapped to NIL, (item NIL NIL) to item,
and item(left,right) to (item left right)."
  (when (> (length str) 0)
    (if (char= (char str (1- (length str))) #\))
        (do* ((p1 (position #\( str))
              (depth 0)
              (p (1+ p1) (1+ p)))
             ((or (null p1) (>= p (length str))))
             (and (= depth 0)
                  (char= (char str p) #\,)
               (return-from NIL
                 (list (intern (string-upcase (subseq str 0 p1)))
                       (string-to-tree (subseq str (1+ p1) p))
                       (string-to-tree (subseq str (1+ p) (1- (length str)))))))
             (case (char str p)
               (#\( (incf depth))
               (#\) (decf depth))))
        (list (intern (string-upcase str)) NIL NIL))))

;;; p68

(defun inorder (tree)
  "Construct the inorder sequence of the given tree."
  (when tree
    (nconc (inorder (second tree))
      (cons (first tree) 
            (inorder (third tree))))))

(defun preorder (tree)
  "Construct the preorder sequence of the given tree."
  (when tree
    (cons (first tree)
      (nconc (preorder (second tree))
             (preorder (third tree))))))

(defun pre-in-tree (pre in)
  "Construct tree from its preorder and inorder representations."
  ;; check if pre and in have the same elements
  (and pre in 
       (= (length (union pre in))
          (min (length pre) (length in)))
       (let* ((p1 (position (first pre) in)) ;where the left branch ends in in
              (p2 (position (first (last (subseq in 0 p1))) pre))) ;where it ends in pre
         (list (first pre)
               (and p2 (pre-in-tree (subseq pre 1 (1+ p2)) (subseq in 0 p1)))
               (and p1 p2 (pre-in-tree (subseq pre (1+ p2)) (subseq in (1+ p1))))))))

;;; p69

(defun tree-to-dotstring (tree)
  "Convert tree to dotstring format, which contains the preorder sequence of the nodes of the tree 
with a dot for every instance of NIL encountered. For example, the tree a(b(d,e),c(,f(g,))) as represented in P67
becomes 'abd..e..c.fg...' in dotstring notation."
  (if tree
      (format nil "~a~a~a" (first tree)
        (tree-to-dotstring (second tree))
        (tree-to-dotstring (third tree)))
      #\.))

(defun dotstring-to-tree (str)
  "Convert dotstring to tree, where the dotstring contains the preorder sequence of the nodes of the tree 
with a dot for every instance of NIL encountered. For example, the tree a(b(d,e),c(,f(g,))) as represented in P67
becomes 'abd..e..c.fg...' in dotstring notation."
  ;; treat it like postfix notation in reverse
  (do ((i (1- (length str)) (1- i))
       (stack NIL))
      ((< i 0) (pop stack))
      (case (char str i)
        (#\. (push NIL stack))
        (T (push (list (intern (make-string 1 :initial-element (char-upcase (char str i))))
                       (pop stack)
                       (pop stack))
                 stack)))))
