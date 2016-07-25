;;;; Prime number arithmetic

(in-package :99-lisp-problems)

;;; p31

;; perhaps adapt this to Eratosthenes' sieve using optional arguments
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

;;; p37

(defun phi-improved (n)
  "Calculate Euler's totient function phi(m) (improved).
See problem P34 for the definition of Euler's totient function. If the list of the prime factors of a number m is 
known in the form of problem P36 then the function phi(m) can be efficiently calculated as follows: 
Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m. 
Then phi(m) can be calculated with the following formula:
phi(m) = (p1 - 1) * p1 ** (m1 - 1) + (p2 - 1) * p2 ** (m2 - 1) + (p3 - 1) * p3 ** (m3 - 1) + ..."
  (reduce #'* ;phi is multiplicative
    (mapcar (lambda (pair &aux (p (car pair)) (m (cadr pair)))
              (* (1- p) (expt p (1- m))))
      (prime-factors-mult n))))

;;; p38

(defun compare-phi (n)
  "Compare the two methods of calculating Euler's totient function.
Use the solutions of problems P34 and P37 to compare the algorithms."
  (format t "~%Evaluating phi(~D) using the solution for P34:" n)
  (time (totient-phi n))
  (format t "~%Evaluating phi(~D) using the solution for P37:" n)
  (time (phi-improved n))
  NIL)

;;; p39

(defun list-primes (a b)
  "A list of prime numbers.
Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range."
  (remove-if-not #'prime-p (range a b))) ;p22
  
;;; p40

(defun goldbach (n)
  "Goldbach's conjecture.
Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. 
Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the 
general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Lisp system). 
Write a predicate to find the two prime numbers that sum up to a given even integer.
Example:
* (goldbach 28)
(5 23)"
  (and (evenp n) (> n 2)
    (dolist (p (list-primes 2 n))
      (when (prime-p (- n p))
        (return-from NIL (list p (- n p)))))))

;;; p41

(defun goldbach-list (a b &optional (threshold 0))
  "A list of Goldbach compositions.
Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
Example:
* (goldbach-list 9 20)
10 = 3 + 7
12 = 5 + 7
14 = 3 + 11
16 = 3 + 13
18 = 5 + 13
20 = 3 + 17

In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. 
Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.

Example (for a print limit of 50):
* (goldbach-list 1 2000 50)
992 = 73 + 919
1382 = 61 + 1321
1856 = 67 + 1789
1928 = 61 + 1867"
  (mapc (lambda (n &aux (res (goldbach n)))
          (when (> (car res) threshold)
            (format t "~%~D = ~D + ~D" n (car res) (cadr res))))
    (range a b)))
