(defpackage :zisp.math.zeller
  (:use :cl))
(in-package :zisp.math.zeller)

(defun zeller-float (month day year)
  (let* ((q day)
	 (m month)
	 (k (mod year 100))
	 (j (multiple-value-bind (century)
		(floor (/ year 100)) century))
    (answer (float (mod (+ q
	    (floor (/ (* (+ m 1) 26) 10))
	    k
	    (floor (/ k 4))
	    (floor (/ j 4))
	    (* 2 j)) 7))))
    (values answer)))

(defun zeller-iso (month day year)
  (+ (zeller-float month day year) 1))

(defun zeller (month day year)
  (let ((answer (zeller-iso month day year)))
    (cond ((= answer 1.0) "Sunday")
	  ((= answer 2.0) "Monday")
	  ((= answer 3.0) "Tuesday")
	  ((= answer 4.0) "Wednesday")
	  ((= answer 5.0) "Thursday")
	  ((= answer 6.0) "Friday")
	  ((= answer 7.0) "Saturday")
	  (t "The syntax is (month date year)"))))