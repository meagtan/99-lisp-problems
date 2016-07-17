;;;; An arithmetic puzzle

;;; p93

(defun find-equations (numbers)
  "Given a list of integer numbers, find ways of inserting operators such that the result is a correct equation. 
Example: With the list (2 3 5 7 11) we can form the equations (2 - 3 + 5 + 7 = 11) or (2 = [ 3 * 5 + 7 ] / 11)."
  ;; break down position of = sign
  (when numbers
    (remove-duplicates
      (mapcar #'prefix-to-infix
        (mapcan (lambda (i &aux (sides (split numbers i))) ;p17
                  ;; consider negations as well (reducing them later is much more cumbersome)
                  (append (make-equations (car sides) (cadr sides))
                          (make-equations (cons (- (caar sides)) (cdar sides))
                                          (cons (- (caadr sides)) (cdadr sides)))
                          ))
          (range 1 (1- (length numbers))))) ;p22
      :test #'equal)))

(defun make-equations (lhs rhs)
  "Return all equations whose left-hand side is formed from LHS and right-hand side from RHS by inserting operators."
  (if (< (length lhs) (length rhs))
      (mapcan (lambda (lhs-expr)
                (mapcar (lambda (rhs-expr)
                          (list '= lhs-expr rhs-expr))
                        ;; remove all but the expressions whose value is the same as lhs-expr
                        (remove (eval lhs-expr) (make-exprs rhs)
                          :test-not #'eql
                          :key #'eval)))
              (make-exprs lhs))
      (mapcan (lambda (rhs-expr)
                (mapcar (lambda (lhs-expr)
                          (list '= lhs-expr rhs-expr))
                        ;; remove all but the expressions whose value is the same as rhs-expr
                        (remove (eval rhs-expr) (make-exprs lhs)
                          :test-not #'eql
                          :key #'eval)))
              (make-exprs rhs))))

(defun make-exprs (numbers)
  "Generate all expressions formed by inserting operators to NUMBERS."
  (if (= (length numbers) 1)
      numbers ;(apply-unary-ops (car numbers))
      ;; split numbers, combine each expression from either partition using a binary operator
      (loop for i from 1 to (1- (length numbers))
            for partition = (split numbers i) append 
          (loop for arg1 in (make-exprs (car partition)) append 
              (loop for arg2 in (make-exprs (cadr partition)) append 
                (apply-binary-ops arg1 arg2))))))

(defun apply-unary-ops (arg1)
  "Apply unary operations to argument."
  (list arg1 (list '- arg1)))

(defun apply-binary-ops (arg1 arg2)
  "Apply binary operations to arguments."
  (loop for op in '(+ - * /)
    if (apply-op op arg1 arg2)
    collect it))

(defun apply-op (op arg1 arg2)
  "Apply operator to arguments, disregarding associativity."
  (unless (and (eq op '/) (zerop (eval arg2)))
    (list op arg1 arg2)))

;;; An aside: converting from lispy arithmetic expressions to infix notation
;;; Not necessary for the purpose of the program, as the order of numbers stays the same, but truer to the original problem.

(defparameter *unary-ops*  '(-) "Unary operators.")
(defparameter *binary-ops* '(= + - * /) "Binary operators, sorted by precedence.")
(defparameter *op-complements* '((-  +) (/  *)) "Match binary operators to the operators they are a complement of.")

(defun prefix-to-infix (expr)
  "Convert a Lisp arithmetic expression in Polish notation into a list of numbers and operators in infix notation.
Example: (+ (* 3 5) (- 2 7)) => (3 * 5 + 2 - 7); (* (+ 3 5) (- 2 7)) => ([ 3 + 5 ] * [ 2 - 7 ])."
  (cond ((numberp expr) (list expr))
        ((unary-p expr) expr)
        ((binary-p expr)
         (append (infix-arg (first expr) (second expr) T)
                 (cons (first expr)
                   (infix-arg (first expr) (third expr) NIL))))))

(defun infix-arg (op expr lhs &aux (res (prefix-to-infix expr)))
  "Optionally add parentheses to the infix notation of EXPR, as an argument to OP."
  (if (or (not (binary-p expr))
          (precedes-p op (first expr) lhs))
      res
      (cons '[ (append res (list '])))))

(defun precedes-p (op1 op2 lhs)
  "Return T if an expression of OP2 can be written without parentheses at the LHS/RHS of an expression of OP1."
  (or (< (position op1 *binary-ops*)
         (position op2 *binary-ops*))
      (if (assoc op1 *op-complements*)
          (and lhs
               (member op2 (assoc op1 *op-complements*)))
          (eq op1 op2))))

(defun unary-p (expr)
  "Return T if EXPR is the application of a unary operator."
  (and (listp expr)
       (= (length expr) 2)
       (member (first expr) *unary-ops*)))

(defun binary-p (expr)
  "Return T if EXPR is the application of a binary operator."
  (and (listp expr)
       (= (length expr) 3)
       (member (first expr) *binary-ops*)))

(defun infix-to-prefix (expr)
  "Convert an infix arithmetic expression, expressed as a list of numbers, operators and brackets, into a Lisp expression."
  NIL
  )
