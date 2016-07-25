;;;; More list operations

(in-package #:99-lisp-problems)

;;; p14

(defun dupli (list)
  "Duplicate the elements of a list.
Example:
* (dupli '(a b c c d))
(A A B B C C C C D D)"
  (when list
    (cons (car list)
          (cons (car list)
                (dupli (cdr list))))))
                
;;; p15

(defun repli (list n)
  "Replicate the elements of a list a given number of times.
Example:
* (repli '(a b c) 3)
(A A A B B B C C C)"
  (when list
    (add-item (car list) 
              (repli (cdr list) n)
              n)))
              
(defun add-item (item list n)
  "Add ITEM to LIST N times."
  (if (= n 0)
      list
      (add-item item (cons item list) (1- n))))
      
;;; p16

(defun drop (list n &optional (k n))
  "Drop every N'th element from a list.
Example:
* (drop '(a b c d e f g h i k) 3)
(A B D E G H K)"
  (when list
    (if (= k 1)
        (drop (cdr list) n)
        (cons (car list)
              (drop (cdr list) n (1- k))))))
              
;;; p17

(defun split (list n)
  "Split a list into two parts; the length of the first part is given.
Do not use any predefined predicates.

Example:
* (split '(a b c d e f g h i k) 3)
((A B C) (D E F G H I K))"
  (when list
    (if (= n 1)
        (list (list (car list))
              (cdr list))
        (let ((res (split (cdr list) (1- n))))
          (cons (cons (car list) (car res))
                (cdr res))))))
                
;;; p18

(defun slice (list i k)
  "Extract a slice from a list.
Given two indices, I and K, the slice is the list containing the elements between the I'th and 
K'th element of the original list (both limits included). Start counting the elements with 1.

Example:
* (slice '(a b c d e f g h i k) 3 7)
(C D E F G)"
  (when list
    (if (= i 1)
        (if (= k 1)
            (list (car list))
            (cons (car list)
                  (slice (cdr list) 1 (1- k))))
        (slice (cdr list) (1- i) (1- k)))))

;;; p19

(defun rotate (list n)
  "Rotate a list N places to the left.
Examples:
* (rotate '(a b c d e f g h) 3)
(D E F G H A B C)

* (rotate '(a b c d e f g h) -2)
(G H A B C D E F)"
  (when (< n 0)
    (incf n (length list)))
  (when list
    (let ((partition (split list n)))
      (append (cadr partition) (car partition)))))
    
;;; p20

(defun remove-at (list k)
  "Remove the K'th element from a list.
Example:
* (remove-at '(a b c d) 2)
(A C D)"
  (if (= k 1)
      (cdr list)
      (cons (car list)
            (remove-at (cdr list) (1- k)))))
            
;;; p21

(defun insert-at (item list k)
  "Insert an element at a given position into a list.
Example:
* (insert-at 'alfa '(a b c d) 2)
(A ALFA B C D)"
  (if (= k 1)
      (cons item list)
      (cons (car list)
            (insert-at item (cdr list) (1- k)))))
