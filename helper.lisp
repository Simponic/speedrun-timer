(ql:quickload "cl-charms")

;; Splits are in the following format
;; (name elapsed-time-for-split best-split split-pb)
(defvar *interval* internal-time-units-per-second)
(defvar *splits*
  '(
	("Chozo" 0 0 0)
	("Kraid" 0 0 0)
	("Wave Beam" 0 0 0)
	("Phantoon" 0 0 0)
	("Botwoon" 0 0 0)
	("Draygon" 0 0 0)
	("Lower Norfair" 0 0 0)
	("Ridley" 0 0 0)
	("Mother Brain" 0 0 0)))
(defvar *all-splits* '())

(defun set-val-in-list (list index value)
  (cond
	((null list) '())
	((zerop index) (cons value (cdr list)))
	(t (cons (car list) (set-val-in-list (cdr list) (1- index) value)))))

(defun get-val-in-list (list index)
  (cond
	((null list) nil)
	((zerop index) (car list))
	(t (get-val-in-list (cdr list) (1- index)))))

(defun push-on-list (value list)
  (cond
	((null list) (list value))
    (t (cons (car list) (push-on-list value (cdr list))))))

(defun read-file-of-splits (filename)
  (with-open-file (in filename)
	  (with-standard-io-syntax
	    (setf *all-splits* (read in)))))

(defun write-file-of-splits (filename splits)
  (with-open-file (output filename)
	:direction :output
	:if-exists :supersede
	(with-standard-io-syntax
	  (print splits output))))

(defun print-character-times (n char)
  (cond
	((zerop n) "")
	(t
	  (concatenate 'string (format nil "~a" char) (print-character-times (1- n) char)))))

(defun center-string (string length)
  (concatenate
   'string
   (print-character-times (floor (/ (- length (length string)) 2)) " ")
   (format nil "~a" string)
   (print-character-times (ceiling (/ (- length (length string)) 2)) " ")))

(defun time-to-milliseconds (time)
  (* (/ time *interval*) 1000))

(defun format-time (milliseconds)
  (let*
	  ((hours   (floor (/ milliseconds (* 1000 60 60))))
	   (minutes (floor (mod (/ milliseconds (* 1000 60)) 60)))
	   (seconds (floor (mod (/ milliseconds 1000) 60)))
	   (centis (mod (floor (/ milliseconds 10)) 100)))
	(format
	 nil "~a"
	 (concatenate
	  'string
	  (cond ((zerop hours) "") (t (format nil "~2,'0D:" hours)))
	  (cond ((zerop minutes) "") (t (format nil "~2,'0D:" minutes)))
	  (format nil "~2,'0D." seconds)
	  (format nil "~2,'0D" centis)))))

(defun format-split (split)
  (cond
	((null split) "")
	(t
	 (concatenate
	  'string
	   (cond
		 ((numberp (car split))
		  (format nil "~a|" (center-string (format-time (car split)) 12)))
		 (t (format nil "|~a|" (center-string (car split) 15))))
	  (format-split (cdr split))))))  
