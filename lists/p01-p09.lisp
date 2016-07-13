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
  (and list (cdr list)
    (if (cddr list)
        (my-but-last (cdr list)))
        list)))
        
;;; p03
(defun my-length (list)
  "Find the number of elements of a list."
  (if list
      (1+ (my-length (cdr list)))
      0))
  
