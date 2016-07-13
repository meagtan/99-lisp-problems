;;;; Further index operations

;;; p22
(defun range (i k)
  "Create a list containing all integers within a given range.
If first argument is smaller than second, produce a list in decreasing order.
Example:
* (range 4 9)
(4 5 6 7 8 9)"
  (cond ((= i k) (list i))
        ((< i k)
         (reverse (range k i)))
        (T
         (cons i (range (1+ i) k)))))
         
;;; p23
(defun rnd-select (list k)
  "Extract a given number of randomly selected elements from a list.
The selected items shall be returned in a list.
Example:
* (rnd-select '(a b c d e f g h) 3)
(E D A)"
  (when (> k 0)
    (let ((i (random (length list))))
      (cons (element-at list i) ;p03
            (rnd-select (remove-at list i) ;p20
                        (1- k))))))
                        
;;; p24
(defun lotto-select (n m)
  "Lotto: Draw N different random numbers from the set 1..M.
The selected numbers shall be returned in a list.
Example:
* (lotto-select 6 49)
(23 1 17 33 21 37)"
  (rnd-select (range 1 m) ;p22
              n)) ;p23

;;; p25
(defun rnd-permu (list)
  "Generate a random permutation of the elements of a list.
Example:
* (rnd-permu '(a b c d e f))
(B A D C E F)"
  (rnd-select list (length list))) ;p23
  
;;; p26
(defun combination (k list)
  "Generate the combinations of K distinct objects chosen from the N elements of a list
In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

Example:
* (combination 3 '(a b c d e f))
((A B C) (A B D) (A B E) ... )"
  (cond ((null list) NIL)
        ((= k 0) (list NIL))
        (T (append (combination k (cdr list))
                   (mapcar (lambda (c) (cons (car list) c))
                           (combination (1- k) (cdr list)))))))


;;; p27
(defun group3 (list)
  "Group the elements of a set into disjoint subsets.
a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? 
Write a function that generates all the possibilities and returns them in a list.

Example:
* (group3 '(aldo beat carla david evi flip gary hugo ida))
( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
... )"
  (group list '(2 3 4)))

(defun group (list sizes)
  "Group the elements of a set into disjoint subsets.
b) Generalize the above predicate in a way that we can specify a list of group sizes 
and the predicate will return a list of groups.

Example:
* (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
... )"
  (mapcar (lambda (siz) (partition list siz)) (permutations sizes)))

(defun partition (list sizes)
  "Partition list into sublists of size given in sizes, assuming (apply #'+ sizes) = (length list)."
  (if (= (length sizes) 1)
      (list list)
      (let ((res (split list (car sizes))))
        (list (car res)
              (partition res (cdr sizes))))))

(defun permutations (list)
  "Generate all the different permutations of LIST, accounting for reoccurring terms."
  (
