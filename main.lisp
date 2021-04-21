(ql:quickload "cl-charms")
(ql:quickload "trivial-left-pad")

(defvar *splits* '(("Chozo" 0 0 0)("Kraid" 0 0 0)("Wave Beam" 0 0 0)("Phantoon" 0 0 0)("Botwoon" 0 0 0)("Draygon" 0 0 0)("Ridley" 0 0 0)("Mother Brain" 0 0 0)))
(defvar *current-split-index* 0)
(defvar *interval* internal-time-units-per-second)
(defvar *start-time* 0)

(defun current-time ()
  (let ((time (get-internal-real-time)))
	(cond ((zerop *start-time*) (setf *start-time* time))
		  (t (- time *start-time*)))))

(defun time-to-millis (time)
  (* (/ time *interval*) 1000))

(defun get-value (list index)
  (cond
	((null list) nil)
	((zerop index) (car list))
	(t (get-value (cdr list) (1- index)))))

(defun change-value (list index value)
  (cond
	((null list) '())
	((zerop index) (setq list (cons value (cdr list))))
	(t (setq list (cons (car list) (change-value (cdr list) (1- index) value))))))

(defun add-to-string-if-not-empty (string suffix)
  (cond ((not (zerop (length string))) (concatenate 'string string suffix))))

(defun number->two-wide (num)
  (cond ((not (zerop num)) (format nil "~2,'0D" num)) (t "")))

(defun millis->strings (millis)
  (let*
	((hours   (/ millis (* 1000 60 60)))
	 (minutes (mod (/ millis (* 1000 60)) 60))
	 (seconds (mod (/ millis 1000) 60))
	 (centis  (mod millis 100)))
	(list
	 (number->two-wide (floor hours))
	 (number->two-wide (floor minutes))
	 (format nil "~2,'0d" (floor seconds))
	 (format nil "~2,'0d" (floor centis)))))

(defun time->string (time_strs)
  (concatenate 'string
   (add-to-string-if-not-empty (car   time_strs) ":")
   (add-to-string-if-not-empty (cadr  time_strs) ":")
   (add-to-string-if-not-empty (caddr time_strs) ".")
   (cadddr time_strs)))

(defun format-split (split)
  (cond
	((null split) "")
	(t
	 (concatenate
	  'string
	  (trivial-left-pad:left-pad
	   (cond
		 ((numberp (car split))
		  (time->string (millis->strings (car split))))
		 (t (car split)))
	  15)
	  (format-split (cdr split))))))

(defun start-split (split)
  (setq split (change-value split 1 (time-to-millis (current-time))))
  (setq split (change-value split 2 (time-to-millis (current-time)))))

(defun update-split (split)
  (cond
	((zerop (caddr split)) (setq split (start-split split)))
  )
  (setq split (change-value split 2 (time-to-millis (current-time))))
  (setq split (change-value split 3 (- (caddr split) (cadr split)))))

(defun format-splits (current_list)
  (cond
	((null current_list) "")
	(t
	 (concatenate
	  'string
	  (format nil "|~a|~%" (format-split (car current_list)))
	  (format-splits (cdr current_list)))))
  )
   
(defun do-on-current-split (f)
  (setq
   *splits*
   (change-value
	*splits*
	*current-split-index*
	(funcall f
	 (get-value
	  *splits*
	  *current-split-index*)))))

(defun hello-world ()
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-non-blocking-mode charms:*standard-window*)
	(setq *start-time* (get-internal-real-time))
	(loop :named driver-loop
		  :for c := (charms:get-char charms:*standard-window*
									 :ignore-error t)
		  :do (progn
				(charms:clear-window charms:*standard-window*)
				(cond ((null (get-value *splits* *current-split-index*)) (return-from driver-loop)))
				(do-on-current-split (lambda (x) (update-split x)))			
				(charms:write-string-at-point charms:*standard-window*(format-splits *splits*) 0 0)
				(charms:refresh-window charms:*standard-window*)
				(case c
				  ((nil) nil)
				  ((#\Space) (incf *current-split-index* 1))
				  ((#\q) (return-from driver-loop)))
				(sleep 0.01)
				)))
  (get-value *splits* (1- *current-split-index*))
)

