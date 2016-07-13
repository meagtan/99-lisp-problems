;;;; Logic and codes

;;; p46-48

;;; The original Prolog problems rest on language-specific features such as operators. The following functions will concern 
;;;   logical expressions formed from predicates, represented as symbols, using operators not, and, or, nand, nor, xor, impl
;;;   and equ.

(defmacro table (preds expr &optional prefs)
  "Print truth table for the given logical expression formed from the given predicates. 
Example:
* (table (A B C) (equ (and A (or B C)) (or (and A B) (and A C))))
true true true true
true true fail true
true fail true true
true fail fail true
fail true true true
fail true fail true
fail fail true true
fail fail fail true"
  (if (null preds)
      `(format t "~%~{~a~^ ~} ~a" ,(reverse prefs) (bool-symbol ,expr))
      (list 'progn
        (table (cdr preds) (subst T   (car preds) expr) (cons (bool-symbol T)   prefs))
        (table (cdr preds) (subst NIL (car preds) expr) (cons (bool-symbol NIL) prefs)))))

(defun bool-symbol (bool)
  (if bool 'true 'fail))

;; Logical operations

(defun nand (a b) (not (and a b)))
  
(defun nor (a b) (not (or a b)))
  
(defun xor (a b) (if a (not b) b))

(defun impl (a b) (if a b T))

(defun equ (a b) (if a b (not b)))

;;; p49

(let (dict) ;for memoization
  (defun gray (n)
    "Return the N-bit Gray code, memoized for each N."
    (cond ((= n 1)
           (list (list 0) (list 1))) ;literal value, no need to memoize
          (t 
           (unless (assoc n dict) ;if not memoized, memoize
             (push (list n
                     (append (mapcar (lambda (code) (cons 0 code)) (gray (1- n)))
                             (mapcar (lambda (code) (cons 1 code)) (reverse (gray (1- n))))))
                   dict))
           (cadr (assoc n dict))))))

;;; p50

(defun huffman (fs)
  "Return the Huffman code table, represented as a list of pairs, given a frequency table also represented as a list of pairs."
  (setf fs (copy-tree fs)) ;destructive operations applied to fs
  ;; convert fs to binary tree
  (do () ((= (length fs) 1))
      ;; sort items based on frequency
      (sort fs #'< :key #'cdr)
      ;; combine the two least frequent items
      (setf fs (cons (cons (cons (caar fs) (caadr fs)) ;collect the items of the first two pairs
                           (+ (cdar fs) (cdadr fs)))
                     (cddr fs))))
  (generate-table (caar fs)))

(defun generate-table (tree)
  "Generate Huffman code table from binary tree, represented as a pair containing a tree and a frequency value."
  (if (consp tree) ;on a node
      (append (mapcar (lambda (pair) (cons (car pair) (cons 0 (cdr pair)))) 
                (generate-table (car tree)))
              (mapcar (lambda (pair) (cons (car pair) (cons 1 (cdr pair)))) 
                (generate-table (cdr tree))))
      (list (cons tree NIL))))
