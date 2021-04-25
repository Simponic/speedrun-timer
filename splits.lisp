(load "helper.lisp")

(defun sum-splits (splits at-index)
  (cond ((null splits) 0)
		(t (+ (get-val-in-list (car splits) at-index) (sum-splits (cdr splits) at-index)))))

(defun get-best-run (list-of-runs current-best-run current-minimum)
  (cond
	((null list-of-runs) current-best-run)
	(t
	 (let ((current-sum (sum-splits (car list-of-runs) 1)))
	   (cond
		 ((< current-sum current-minimum) (get-best-run (cdr list-of-runs) (car list-of-runs) (cadr (car list-of-runs))))
		 (t (get-best-run (cdr list-of-runs) current-best-run current-minimum)))))))

(defun get-best-split (list-of-runs split-index current-minimum-split current-minimum-time)
  (cond
	((null list-of-runs) current-minimum-split)
	(t
	 (let*
		 ((current-split (get-val-in-list (car list-of-runs) split-index))
		  (current-time (get-val-in-list current-split 1)))
	   (cond
		 ((< current-time current-minimum-time)
		  (get-best-split (cdr list-of-runs) split-index current-split current-time))
		 (t
		  (get-best-split (cdr list-of-runs) split-index current-minimum-split current-minimum-time)))))))

(defun make-run-of-best-segments (list-of-runs index)
  (cond
	((null list-of-runs) '())
	((null (get-val-in-list (car list-of-runs) index)) '())
	(t
	 (cons
	  (get-best-split list-of-runs index (get-val-in-list (car list-of-runs) index) (sum-splits (car list-of-runs ) 1))
	  (make-run-of-best-segments list-of-runs (1+ index))))))

(defun update-split (current-split start-time)
  (set-val-in-list
   current-split
   1
   (time-to-milliseconds (- (get-internal-real-time) start-time))))


(defun set-column-in-run-from-other-run (column_index run_to_copy list-of-splits)
  (cond
	((null list-of-splits) nil)
	(t
	 (cons
	  (set-val-in-list
	   (car list-of-splits) column_index (get-val-in-list (car run_to_copy) 1))
	  (set-column-in-run-from-other-run
	   column_index (cdr run_to_copy) (cdr list-of-splits))))))

(defun make-output (splits current-sum pb)
  (cond
	((null splits)
	 (format nil "~%| Current time: |~a|~%|      PB:      |~a|" (center-string (format-time current-sum) 12) (center-string (format-time pb) 12)))
	(t
	 (concatenate
	  'string
	  (format nil "~a~%" (format-split (car splits)))
	  (make-output
	   (cdr splits)
	   (+ current-sum (get-val-in-list (car splits) 1))
	   pb)))))

