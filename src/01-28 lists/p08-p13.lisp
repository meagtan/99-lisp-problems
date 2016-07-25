;;;; Compression, packing, etc.

(in-package :99-lisp-problems)

;;; p08

(defun compress (list)
  "Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. 
The order of the elements should not be changed.

Example:
* (compress '(a a a a b c c a a d e e e e))
(A B C A D E)"
  (if (cdr list)
      (if (eq (car list) (cadr list))
          (compress (cdr list))
          (cons (car list) (compress (cdr list))))
      list))
      
;;; p09

(defun pack (list)
  "Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.

Example:
* (pack '(a a a a b c c a a d e e e e))
((A A A A) (B) (C C) (A A) (D) (E E E E))"
  (if (cdr list)
      (let ((res (pack (cdr list))))
        (if (eq (car list) (cadr list))
            (cons (cons (car list) (car res)) (cdr res))
            (cons (list (car list)) res)))
      (when list (list list))))

;;; p10

(defun encode (list)
  "Run-length encoding of a list.
Use the result of problem P09 to implement the so-called run-length encoding data compression method. 
Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

Example:
* (encode '(a a a a b c c a a d e e e e))
((4 A) (1 B) (2 C) (2 A) (1 D) (4 E))"
  (mapcar (lambda (sublist) (list (my-length sublist) ;p04
                                  (car sublist)))
          (pack list))) ;p09
          
;;; p11

(defun encode-modified (list)
  "Modified run-length encoding.
Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. 
Only elements with duplicates are transferred as (N E) lists.

Example:
* (encode-modified '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))"
  (mapcar (lambda (sublist)
            (if (= (car sublist) 1)
                (cadr sublist)
                sublist))
          (encode list))) ;p11
          
;;; p12

(defun decode (list)
  "Decode a run-length encoded list.
Given a run-length code list generated as specified in problem P11. Construct its uncompressed version."
  (my-flatten ;p07
    (mapcar (lambda (sublist)
              (if (atom sublist)
                  (list sublist)
                  (repli (cdr sublist) (car sublist)))) ;p15
            (encode-modified list)))) ;p11
            
;;; p13

(defun encode-direct (list)
  "Run-length encoding of a list (direct solution).
Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem P09, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

Example:
* (encode-direct '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))"
  (if (cdr list)
      (let ((res (encode-direct (cdr list))))
        (cond ((and (atom (car res))
                    (eq (car list) (car res)))
               (cons (list 2 (car list)) (cdr res)))
              ((eq (car list) (cadar res))
               (cons (list (1+ (caar res)) (car list))
                     (cdr res)))))
      list))
