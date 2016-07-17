;;;; An arithmetic puzzle

(defun find-equations (numbers)
  "Given a list of integer numbers, find ways of inserting operators such that the result is a correct equation. 
Example: With the list (2 3 5 7 11) we can form the equations (= (+ (- 2 3) 5 7) 11) or (= 2 (/ (+ (* 3 5) 7) 11))."
  ;; break down position of = sign
  (when numbers
    (mapcan (lambda (i &aux (sides (split numbers i))) ;p17
              (make-equations (car sides) (cadr sides)))
      (range 1 (1- (length numbers)))))) ;p22

(defun make-equations (lhs rhs)
  "Return all equations whose left-hand side is formed from LHS and right-hand side from RHS by inserting operators."
  (if (< (length lhs) (length rhs))
      (mapcan (lambda (lhs-expr)
                (mapcar (lambda (rhs-expr)
                          (list '= lhs-expr rhs-expr))
                        ;; remove all but the expressions whose value is the same as lhs-expr
                        (remove lhs-expr (make-exprs rhs)
                          :test-not #'eql
                          :key #'eval))
              (make-exprs lhs))
      (mapcan (lambda (rhs-expr)
                (mapcar (lambda (lhs-expr)
                          (list '= lhs-expr rhs-expr))
                        ;; remove all but the expressions whose value is the same as rhs-expr
                        (remove rhs-expr (make-exprs lhs)
                          :test-not #'eql
                          :key #'eval))))
              (make-exprs rhs))))

(defun make-exprs (numbers)
  "Generate all expressions formed by inserting operators to NUMBERS."
  (if (= (length numbers) 1)
      `(,(car numbers) (- ,(car numbers)))
      ;; split numbers, combine each expression from either partition using a binary operator
      ))
