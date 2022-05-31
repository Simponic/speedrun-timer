;; Makes a-list with '((hours . HOURS) (minutes . MINUTES) (seconds . SECONDS) (cs . CENTISECONDS))
(defun make-time-alist (cs)
  `((hours    . ,(floor (/ cs (* 100 60 60))))
	  (minutes . ,(floor (mod (/ cs (* 100 60)) 60)))
	  (seconds . ,(floor (mod (/ cs 100) 60)))
	  (cs      . ,(mod cs 100))))

;; Formats, disregarding min/hour if they shouldn't be shown, a time alist to "H:M:S.ms"
(defun format-time (time-alist)
  (let
      ((hours (cdr (assoc 'hours time-alist)))
       (minutes (cdr (assoc 'minutes time-alist)))
       (seconds (cdr (assoc 'seconds time-alist)))
       (centis (cdr (assoc 'cs time-alist))))
    (concatenate 'string
                 (unless (zerop hours) (format nil "~2,'0d:" hours))
                 (unless (and (zerop minutes) (zerop hours)) (format nil "~2,'0d:" minutes))
                 (format nil "~2,'0d.~2,'0d" seconds centis))))
