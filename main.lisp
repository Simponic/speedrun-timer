(load "helper.lisp")
(load "splits.lisp")

(defvar current-split '())
(defvar done-splits '())
(defvar last-time-space-pressed 0)
(defvar pb 0)

(defun read-in-splits (filename)
  (cond
	((y-or-n-p "Read file")
	  (read-file-of-splits filename)
	  (setf
	   *splits*
	   (set-column-in-run-from-other-run
		2
		(make-run-of-best-segments *all-splits* 0)
		*splits*))
	  (setf
	   *splits*
	   (set-column-in-run-from-other-run
		3
		(get-best-run *all-splits* (car *all-splits*) (cadr (car *splits*)))
		*splits*))))
  (setf pb (sum-splits *splits* 3))
  (setf current-split (car *splits*))
  (setf *splits* (cdr *splits*))
)

(defun main (filename)
  (read-in-splits filename)
  (setf last-time-space-pressed (get-internal-real-time))
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-non-blocking-mode charms:*standard-window*)
	(loop :named driver-loop
		  :for c := (charms:get-char charms:*standard-window*
									 :ignore-error t)
		  :do (progn
				(charms:clear-window charms:*standard-window*)
				(if (null current-split) (return-from driver-loop))
				(charms:write-string-at-point charms:*standard-window* (make-output (push-on-list current-split done-splits) 0 pb) 0 0)
				(charms:refresh-window charms:*standard-window*)
				(setf current-split (update-split current-split last-time-space-pressed))
				(case c
				  ((nil) nil)
				  ((#\Space)
				   (progn
					 (setf last-time-space-pressed (get-internal-real-time))
					 (setf done-splits (push-on-list current-split done-splits))
					 (setf current-split (car *splits*))
					 (setf *splits* (cdr *splits*))))
				  ((#\q) (return-from driver-loop)))
				(sleep 0.01)
				)))
  (setf *all-splits* (cons done-splits *all-splits*))
  (cond ((y-or-n-p "Save?") (with-open-file (output filename :direction :output :if-exists :supersede) (print *all-splits* output))))
  (format nil (format-time (sum-splits done-splits 1))))
