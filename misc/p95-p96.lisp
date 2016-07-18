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
