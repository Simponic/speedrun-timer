;; Makes a-list with '((hours . HOURS) (minutes . MINUTES) (seconds . SECONDS) (ms . MILLISECONDS))
(defun make-time-alist (ms)
  `((hours    . ,(floor (/ ms (* 1000 60 60))))
	   (minutes . ,(floor (mod (/ ms (* 1000 60)) 60)))
	   (seconds . ,(floor (mod (/ ms 1000) 60)))
	   (ms      . ,(mod ms 1000))))

;; Formats, disregarding min/hour if they shouldn't be shown, a time alist to "H:M:S.ms"
(defun format-time (time-alist)
  (let
      ((hours (cdr (assoc 'hours time-alist)))
       (minutes (cdr (assoc 'minutes time-alist)))
       (seconds (cdr (assoc 'seconds time-alist)))
       (centis (round (/ (cdr (assoc 'ms time-alist)) 10))))
    (concatenate 'string
                 (unless (zerop hours) (format nil "~2,'0d:" hours))
                 (unless (and (zerop minutes) (zerop hours)) (format nil "~2,'0d:" minutes))
                 (format nil "~2,'0d.~2,'0d" seconds centis))))
