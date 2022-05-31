(defmacro inc (x &optional (val 1))
  `(setf ,x (+ ,val ,x)))

;; For system arguments
(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun max-length (lists)
  (reduce (lambda (a x) (max a x)) (mapcar #'length lists)))

(defun not-empty-string (str)
  (not (zerop (length str))))
