;;;; Compression, packing, etc.
                      
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

