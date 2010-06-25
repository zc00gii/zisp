(defpackage #:zisp.math.zeller
  (:use :cl))
(in-package :zisp.math.zeller)

(defun syntax (function-name)
  (values (format nil "The syntax is (~a month date year)" function-name)))

(defun zeller-float (month day year &key (name "zeller-float") (errorp nil) (isop nil))
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
    (if isop (if (< answer 7)
		 (+zeller.syntax+ name))
	(if (not isop)
	    (if (< answer 6)
		(values (+zeller.syntax+ name)))))
    (values answer)))

(defun zeller-iso (month day year)
  (+ (zeller-float month day year :isop t :name "zeller-iso") 1))

(defun zeller (month day year)
  (let ((answer (zeller-float month day year :name "zeller")))
    (cond ((= answer 0.0) "Sunday")
	  ((= answer 1.0) "Monday")
	  ((= answer 2.0) "Tuesday")
	  ((= answer 3.0) "Wednesday")
	  ((= answer 4.0) "Thursday")
	  ((= answer 5.0) "Friday")
	  ((= answer 6.0) "Saturday"))))))