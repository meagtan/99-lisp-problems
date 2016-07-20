;;;; Language processing

;;; p95

(defun full-words (number &optional (base 10))
  "Print NUMBER in full words. Example: (full-words 175) => \"one-seven-five\"."
  (format nil "~{~a~^-~}" (mapcar #'digit-word (digits number base))))

(defun digits (number &optional (base 10) acc)
  "Return the digits of NUMBER as a list."
  (if (< number base)
      (cons number acc)
      (multiple-value-bind (div rem) (truncate number base)
        (digits div base (cons rem acc)))))

(defun digit-word (digit)
  "Return the word corresponding to DIGIT."
  (cdr (assoc number *digit-words*)))

(defparameter *digit-words*
  (mapcar (lambda (d)
            (cons d (format nil "~r" d)))
    (range 0 9)) ;p22
  "An alist mapping each digit to their word representation.")
  
;; Addition: printing numbers in words including teens, hundreds and thousands, not digit by digit

(defun number-to-word (number &aux (base 10))
  "Return the word representation of NUMBER ≥ 0. Example: (number-to-word 175) => \"one hundred seventy-five\"."
  (if (and (< number *hundreds-limit*) (/= (rem number (expt base 3)) 0))
      (multiple-value-bind (div rem) (truncate number (expt base 2))
        ;; could be made into a format directive, but clearer this way
        (if (= div 0)
            (tens-to-word rem)
            (format nil "~a hundred~@[ ~a~]"
              (tens-to-word div)
              (when (> rem 0)
                (tens-to-word rem)))))
      (do ((n number)
           (c 0 (1+ c))
           res)
          ((= n 0)
           (format nil "~{~a~^ ~}" res))
          (multiple-value-bind (div rem) (truncate n (expt base 3))
            ;; add rem to list only if nonzero
            (when (/= rem 0)
              (when (> c 0)
                (push (nth (1- c) *thousands*) res))
              (push (number-to-word rem) res))
            ;; if haven't reached the end of thousands
            (if (< c (1- (length *thousands*)))
                (setf n div)
                (progn 
                  (push (car (last *thousands*)) res)
                  (push (number-to-word div) res)
                  (setf n 0)))))))

(defun tens-to-word (number &aux (base 10))
  "Convert 0 ≤ NUMBER ≤ 99 into its word representation. Example: (tens-to-word 43) => \"forty-three\"."
  (multiple-value-bind (div rem) (truncate number base)
    (format nil "~[~a~;~*~a~:;~2*~a-~1@*~a~]"
      div
      (second (assoc rem *digits*))    ;ones,  print if div = 0
      (third  (assoc rem *digits*))    ;teens, print if div = 1
      (fourth (assoc div *digits*))))) ;tens,  print (with ones) if div >= 2

(defparameter *hundreds-limit* 2000
  "Print all numbers less than *HUNDREDS-LIMIT*, except for multiples of 1000, using hundreds instead of thousands.
Example: \"eleven hundred\" for 1100, instead of \"one thousand one hundred\".")

(defparameter *thousands*
  '("thousand" "million" "billion" "trillion" "quadrillion" "quintillion" "sextillion" "septillion" "octillion" "nonillion")
  "List of powers of 1000 up to 1000^30.")

(defparameter *digits*
  (mapcar (lambda (d)
            (list d
                  (format nil "~r" d)
                  (format nil "~r" (+ 10 d))
                  (format nil "~r" (* 10 d))))
    (range 0 9)) ;p22
  "Alist mapping every digit D to the word representation of D, 1D and D0.")

;;; p96

(defun identifier-p (string &aux (list (coerce string 'list)))
  "Return T if STRING represents a valid identifier, represented by the quasi-regex syntax \"<alpha>*(_?[<alpha>|<digit>])*\"."
  (labels ((rest-p (list) 
             (or (null list)
                 (and (alphanumericp (car list))
                      (rest-p (cdr list)))
                 (and (char= (car list) #\_)
                      (cdr list)
                      (alphanumericp (cadr list))
                      (rest-p (cddr list))))))
    (and list
         (alpha-char-p (car list))
         (rest-p (cdr list)))))

;; Extending the above predicate for general syntax diagrams as in the problem

;; A syntax diagram can be represented as a directed graph, expressed in readable list form, that transitions from state to
;;  state based on a recognizer predicate that returns the rest of the given sequence if it is true, and NIL otherwise.

(defun alt-identifier-p (string)
  "Alternate implementation of IDENTIFIER-P using general syntax diagrams."
  (funcall (recognizer-predicate
             (make-recognizer
               `((start a ,(predicate-recognizer #'alpha-char-p))
                 (a b ,(at-most-one (lambda (x) (eq x #\_))))
                 (b end ,(predicate-recognizer #'alphanumericp))
                 (start end ,id-recognizer))))
           string))

(defun make-recognizer (syntax-list &aux (graph (d-readable-graph syntax-list))
                                         (start (start-state graph))
                                         (end (end-state graph)))
  "Create a recognizer predicate from the given list of directed edges of a syntax diagram. 
A valid syntax diagram is composed of nodes, with a unique start node having out-degree 1 and in-degree 0,
and a unique end node having out-degree 0, and directed, labeled edges with recognizer predicates as labels."
  (and start end
    (lambda (string)
      ;; breadth-first graph search
      (do ((node start)
           (string string)
           queue
           visited)
          ((eq node end) ;is this the right ending condition?
           string)
          (let ((added (loop for e in (edges node graph)
                             for res = (funcall (edge-weight e) string)
                         if (not (member (end-node e) visited))
                         if res
                         collect (cons (end-node e) res))))
            (setf queue   (append queue added))
            (setf visited (append visited added)))
          (let ((pair (pop queue)))
            (setf node (car pair)
                  string (cdr pair)))))))

(defun start-state (syntax-graph)
  "Return the starting state of a given syntax diagram, if any."
  (let ((start-list (remove 0
                      (remove 1 (graph-nodes syntax-graph) :test-not #'= :key #'out-degree)
                      :test-not #'= :key #'in-degree)))
    (when (= (length start-list) 1)
      (car start-list))))

(defun end-state (syntax-graph)
  "Return the ending state of a given syntax diagram, if any."
  (let ((end-list (remove 0 (graph-nodes syntax-graph) :test-not #'= :key #'out-degree)))
    (when (= (length end-list) 1)
      (car end-list))))

(defun predicate-recognizer (pred)
  "Generate a recognizer predicate for strings from a predicate for characters."
  (lambda (string)
    (when (funcall pred (char string 0))
          (subseq string 1))))

(defun recognizer-predicate (recognizer)
  "Generate a predicate that tests for exactly the strings recognized by RECOGNIZER."
  (lambda (string &aux (res (funcall recognizer string)))
    (and res (zerop (length res)))))

(defun at-most-one (pred)
  "Generate a recognizer predicate that checks at most one instance for the given predicate for characters."
  (lambda (string)
    (if (funcall pred (char string 0))
        (subseq string 1)
        string)))

(defun kleene-star (pred)
  "Generate a recognizer predicate that checks any number of instances for the given predicate for characters."
  (lambda (string)
    (if (funcall pred (char string 0))
        (funcall (kleene-star pred) (subseq string 1))
        string)))

(defconstant id-recognizer #'identity "A recognizer predicate that does nothing.")

(defun recognizer-union (arg1 arg2)
  "Return the union of two recognizers."
  (lambda (string &aux (res (funcall arg1 string)))
    (if res
        res
        (funcall arg2 string))))

(defun recognizer-compose (arg1 arg2)
  "Return the composition of two recognizers."
  (lambda (string &aux (res (funcall arg1 string)))
    (when res
      (funcall arg2 res))))
