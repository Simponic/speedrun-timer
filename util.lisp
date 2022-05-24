;; For big ascii-art digits
(load "digits.lisp")

(defmacro inc (x)
  `(setf ,x (1+ ,x)))

;; Wraps text and adds ellipsis if it doesn't fit within width, scrolling
;; by index i
(defun maybe-wrap-text (text width i)
  (let ((textlen (length text)))
    (if (>= width textlen)
        text
        (let* ((max-width (1- width))
               (max-wrap (1+ (- textlen max-width)))
               (wrap-i (rem i max-wrap)))
          (concatenate 'string (subseq text wrap-i (+ wrap-i (min max-width textlen))) "-")))))

;; Makes a-list with '((hours . HOURS) (minutes . MINUTES) (seconds . SECONDS) (ms . MILLISECONDS))
(defun make-time-alist (ms)
  `((hours    . ,(floor (/ ms (* 1000 60 60))))
	   (minutes . ,(floor (mod (/ ms (* 1000 60)) 60)))
	   (seconds . ,(floor (mod (/ ms 1000) 60)))
	   (ms      . ,(mod ms 1000))))


;; Add a list of strings representing horizontal slices of a character to another list of strings representing horizontal slices of a string, or create a new list of horizontal slices if the original was empty.
;; Character height will be truncated to the height of the first character in the slices.
(defun add-to-horizontal (character horizontal-layers &key (seperator " "))
  (let ((layer-height (length horizontal-layers)))
    (loop for i from 0 to (1- (if (zerop layer-height) (length character) layer-height))
          collect
          (let ((layer (nth i horizontal-layers))
                (character-slice (nth i character)))
            (if (and layer (> (length layer) 0))
                (concatenate 'string layer seperator character-slice)
              character-slice)))))

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

;; Creates a list of horizontal slices to display a formatted larger string by using figlet characters
(defun lispglet (str &optional (char-hashes *big-digits*))
  (loop for horizontal-layers = '()
        then (add-to-horizontal (gethash c char-hashes) horizontal-layers)
        for c across str
        finally (return horizontal-layers)))
