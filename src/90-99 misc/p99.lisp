;;;; Crossword puzzle solver

(in-package #:99-lisp-problems)

;;; p99

;; The framework of a crossword puzzle is represented internally as a graph, with nodes representing sites, storing 
;;   the coordinates of the starting point, direction and length and (if defined) the contents of the site, and edges 
;;   representing the points at which sites intersect.
;; A site is a list of 4 elements: its starting coordinates, direction (either (cons 1 0) or (cons 0 1)), length and word.
;; An edge has as its weight a dotted pair representing its coordinates.

;; An example string representation of a puzzle is as follows:
;;   ......  .
;;   . .  .  .
;;   . ..... .
;;   . . . ...
;;     . ... .
;;    ...

(defun solve-puzzle (word-list puzzle &aux (graph (puzzle-graph puzzle)))
  "Solve a crossword puzzle, inputted as a string, and return the solved puzzle as a string."
  (when graph
    (assign-words word-list graph)
    (when graph
      (graph-puzzle graph))))

(defun assign-words (word-list graph)
  "Assign words in the given list to each site of GRAPH, in a way compatible with the intersections of each site."
  ;; go through each site, assign words of appropriate length, filter based on intersections
  (when (<= (length (graph-nodes graph))
            (length word-list))
    (do* ((stack (list NIL) (cdr stack)) ;stores alists binding sites to words
          found)
         ((or found (null stack))
          (when found
            (loop for (site word) in found
              do (setf (fourth site) word)
              finally (return T))))
         ;; if (car stack) has a binding for each site
         (if (= (length (car stack))
                (length (graph-nodes graph)))
             (setf found (car stack))
             (setf stack
               (append 
                 (loop with site = (car (set-difference (graph-nodes graph)
                                                        (mapcar #'car (car stack))
                                                        :test #'equal))
                       for word in (set-difference word-list
                                                   (mapcar #'cadr (car stack))
                                                   :test #'equal)
                       if (and (= (third site) (length word))
                               ;; check for intersections
                               (every (lambda (edge &aux (other (other-node edge site)))
                                        (or (null (assoc other (car stack) :test #'equal))
                                            (char= (char word (site-index edge site))
                                                   (char (second (assoc other (car stack) :test #'equal))
                                                         (site-index edge other)))))
                                      (edges site graph))) ;p80
                       collect (cons (list site word) (car stack)))
                 stack))))))

(defun site-index (edge site &aux (start (first site)) (dir (second site)) (meet (edge-weight edge))) ;p80
  "Return the index of the intersection represented by EDGE on SITE, assuming SITE lies on that intersection."
  (+ (* (car dir) (- (car meet) (car start)))
     (* (cdr dir) (- (cdr meet) (cdr start)))))

(defun puzzle-graph (puzzle &aux matrix (graph (make-graph)))
  "Convert the string representation of a puzzle into a graph."
  ;; convert puzzle into 2D matrix for easier horizontal/vertical traversal
  (with-input-from-string (s puzzle)
    (do* ((lst NIL (cons l lst))
          (row 0 (1+ row))
          (col 0 (max col (length l)))
          (l (read-line s NIL) (read-line s NIL))) ;updated last for null check
         ((null l)
          (setf matrix (make-array (list row col)
                         :initial-contents 
                           ;; expand each row to col
                           (mapcar (lambda (row-list)
                                     (append row-list
                                       (make-list (- col (length row-list))
                                         :initial-element #\Space)))
                                   lst))))))
  ;; scan each row and column for sites, add them as nodes to graph
  (add-sites matrix graph (cons 0 1))
  (add-sites matrix graph (cons 1 0))
  ;; check for intersections
  (destructuring-bind
    (horizontal vertical)
    (reduce (lambda (node acc)
              (if (= (car (second node)) 0)
                  (push node (first acc))
                  (push node (second acc))))
            (graph-nodes graph)
            :from-end T
            :initial-element (list NIL NIL))
    (dolist (a horizontal)
      (dolist (b vertical)
        ;; check whether a and b intersect
        ;; if they intersect, they must do so at the row of a and column of b
        (and 
          ;; the column of b is within a's bounds
          (<= 0 
              (- (cdr (first b))
                 (cdr (first a)))
              (1- (third a)))
          ;; the row of a is within b's bounds
          (<= 0 
              (- (car (first a))
                 (car (first b)))
              (1- (third b)))
          (push (list a b (cons (car (first a))
                                (cdr (first b))))
                (graph-edges graph))))))
  graph)

(defun add-sites (matrix graph direction)
  "Scan MATRIX in the direction specified (either horizontal or vertical) and add the sites found to the nodes of GRAPH."
  (dotimes (row (array-dimension matrix (car direction)))   ;row if 0, col if 1
    (do* ((col 0 (1+ col))
          (cell (if (= (car direction) 0) ;horizontal
                    (aref matrix row col)
                    (aref matrix col row)))
          site) ;if a site is being read, contains its starting point, current length and current string
         ((= col (array-dimension matrix (cdr direction)))) ;col if 0, row if 1
         (if (char= cell #\Space)
             ;; if a site has ended, add site to nodes, reset site
             (and site (> (second site) 1) ;so vertical sites aren't captured as well
               (push (list (first site)                  ;starting coordinates
                           direction                     ;the given direction
                           (second site)                 ;length
                           (coerce (third site) 'string) ;word, if any
                     (graph-nodes graph))
               (setf site NIL)))
             ;; start or continue site, with string reset if (aref matrix row col) = #\.
             (cond (site
                    (incf (second site))
                    (if (char= cell #\.)
                        (setf (third site) NIL)
                        (vector-push cell (third site))))
                   (T
                    (setf site
                      (list (cons row col)
                            1
                            (unless (char= cell #\.)
                              (make-array (- (array-dimension matrix 0) row)
                                          :initial-element cell
                                          :fill-pointer 1))))))))))

(defun graph-puzzle (graph &aux matrix)
  "Convert the graph representation of a puzzle into a string."
  ;; create intermediate 2d array
  (destructuring-bind
    (horizontal vertical)
    (reduce (lambda (site acc)
              (if (= (car (second site)) 0)
                  (push site (first acc))
                  (push site (second acc))))
            (graph-nodes graph)
            :from-end T
            :initial-element (list NIL NIL))
    (setf matrix
      (make-array
        ;; get boundaries
        (list (reduce #'max 
                (mapcar (lambda (site)
                          (+ (car (first site)) (1- (third site))))
                        horizontal))
              (reduce #'max 
                (mapcar (lambda (site)
                          (+ (cdr (first site)) (1- (third site))))
                        vertical)))
        :initial-element #\Space)))
  ;; write each site onto matrix
  (dolist (site (graph-nodes graph))
    (type-site site matrix))
  ;; convert matrix into string
  (format nil "~{~A~^~%~}"
    (loop for row to (1- (array-dimension matrix 0))
      collect ;each row as string
        (coerce (make-array (array-dimension matrix 1)
                  :displaced-to matrix
                  :displaced-index-offset
                    (* row (array-dimension matrix 1)))
                'string))))
