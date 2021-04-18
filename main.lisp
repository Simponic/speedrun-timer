(ql:quickload "cl-charms")
(ql:quickload "trivial-left-pad")

(defvar *splits* '(("Kraid" 0 0 0)("Phantoon" 0 0 0)("Draygon" 0 0 0) ("Ridley" 0 0 0)))

(defun make-string-bruh () "bruh")

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

(defun format-my-split (split)
  (cond
	((null split) "")
	(t (concatenate 'string (trivial-left-pad:left-pad (car split) 15) (format-my-split (cdr split))))))

(defun hello-world ()
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input)
    (loop named hello-world
       with window = (charms:make-window 50 15 10 10)
       do (progn
            (charms:clear-window window)
            (charms:write-string-at-point window (make-string-bruh) 0 0)
            (charms:refresh-window window)

            ;; Process input
            (when (eql (charms:get-char window) #\q)
              (return-from hello-world))
            (sleep 0.1)))))
