;;;; Basic list operations

(in-package #:99-lisp-problems)

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
