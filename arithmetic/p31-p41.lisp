;;;; Arithmetic

;;; p31

(defun prime-p (n)
  "Determine whether a given integer number is prime.
Example:
* (prime-p 7)
T"
  (do ((i 2 (1+ i)))
      ((> i (sqrt n)) T)
      (when (divides-p i n) 
        (return-from NIL NIL))))

(defun divides-p (d n)
  "Return T if D divides N."
  (= (rem n i) 0))

;;; p32

(defun gcd (a b)
  "Determine the greatest common divisor of two positive integer numbers.
Use Euclid's algorithm.
Example:
* (gcd 36 63)
9"
  (cond ((< b a) (gcd b a))
        ((= a 0) b)
        (T (gcd (rem b a) a))))

;;; p33

(defun coprime-p (a b)
  "Determine whether two positive integer numbers are coprime.
Two numbers are coprime if their greatest common divisor equals 1.
Example:
* (coprime 35 64)
T"
  (= (gcd a b) 1))

;;; p34

(defun totient-phi (n)
  "Calculate Euler's totient function phi(m).
Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

* (totient-phi 10)
4"
  (do ((d 1 (1+ d))
       (count 0)
      ((> d n) count) ;check >, not >=, to account for n = 1; no difference in result for n > 1
      (when (coprime-p d n)
        (incf count))))

;;; p35

(defun prime-factors (n)
  "Determine the prime factors of a given positive integer.
Construct a flat list containing the prime factors in ascending order.
Example:
* (prime-factors 315)
(3 3 5 7)"
  (do ((p 2 (1+ p)))
      ((> (* p p) n) (list n)) ;no smaller prime divisor found, n is itself prime
      (and (prime-p p)
           (divides-p p n) ;prime divisor found
           (return-from NIL (cons p (prime-factors (/ n p)))))))

;;; p36

(defun prime-factors-mult (n)
  "Determine the prime factors of a given positive integer (2).
Construct a list containing the prime factors and their multiplicity.
Example:
* (prime-factors-mult 315)
((3 2) (5 1) (7 1))"
  (mapcar #'reverse (encode (prime-factors n)))) ;p10
