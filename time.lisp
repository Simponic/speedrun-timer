(defun millis-since-internal-timestamp (start-timestamp &optional (end-timestamp (get-internal-real-time)))
  (ceiling (* 1000 (/ (- end-timestamp start-timestamp) internal-time-units-per-second))))

(defun make-time-alist (millis)
  `((hours    . ,(floor (/ millis (* 1000 60 60))))
	  (minutes  . ,(floor (mod (/ millis (* 1000 60)) 60)))
	  (seconds  . ,(floor (mod (/ millis 1000) 60)))
	  (millis   . ,(mod millis 1000))))

;; Formats, disregarding min/hour if they shouldn't be shown, a time alist to "H:M:S.ms"
(defun format-time (time-alist)
  (let
      ((hours (cdr (assoc 'hours time-alist)))
       (minutes (cdr (assoc 'minutes time-alist)))
       (seconds (cdr (assoc 'seconds time-alist)))
       (centis (floor (/ (cdr (assoc 'millis time-alist)) 10))))
    (concatenate 'string
                 (unless (zerop hours) (format nil "~2,'0d:" hours))
                 (unless (and (zerop minutes) (zerop hours)) (format nil "~2,'0d:" minutes))
                 (format nil "~2,'0d.~2,'0d" seconds centis))))
