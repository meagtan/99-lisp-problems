;;; p01
(defun my-last (list)
  "Find the last box of a list.
Example:
* (my-last '(a b c d))
(D)"
  (when list
    (if (cdr list)
        (my-last (cdr list))
        list)))
        
;;; p02
(defun my-but-last (list)
  "Find the last but one box of a list.
Example:
* (my-but-last '(a b c d))
(C D)"
  (when (cdr list)
    (if (cddr list)
        (my-but-last (cdr list)))
        list)))
        
;;; p03
(defun element-at (list k)
  "Find the K'th element of a list.
The first element in the list is number 1.
Example:
* (element-at '(a b c d e) 3)
C"
  (cond ((= k 1) (car list))
        (list (element-at (cdr list) (1- k)))))
        
;;; p04
(defun my-length (list)
  "Find the number of elements of a list."
  (if list
      (1+ (my-length (cdr list)))
      0))
  
;;; p05
(defun my-reverse (list &optional acc)
  "Reverse a list."
  (if list
      (my-reverse (cdr list) (cons (car list) acc))
      acc))
      
;;; p06
(defun palindrome-p (list)
  "Find out whether a list is a palindrome.
A palindrome can be read forward or backward; e.g. (x a m a x)."
  (equal list (my-reverse list)))
  
;;; p07
(defun my-flatten (list)
  "Flatten a nested list structure.
Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

Example:
* (my-flatten '(a (b (c d) e)))
(A B C D E)"
  (cond ((atom list) (list list))
        (list (append (my-flatten (car list))
                      (my-flatten (cdr list))))))
                      
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
