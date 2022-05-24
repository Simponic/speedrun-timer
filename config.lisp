;; Read a file into a list of lines, trimming whitespace and returning
;; only non-empty lines
(defun read-lines (path)
  (remove-if
   (lambda (s) (equal "" s))
   (mapcar (lambda (s) (string-trim '(#\Space #\Newline #\Tab) s))
   (with-open-file (stream path)
     (loop for line = (read-line stream nil)
           while line
           collect line)))))

;; Returns a list of sections with [name] as first element and all
;; lines of the section as the second containing properties and
;; specs, skipping trailing and preceding whitespace and empty lines
(defun sections (lines &optional (section-list '()) (current-section "") (current-section-list '()))
  (if (not lines)
      (cond
        ((> (length current-section) 0)
         (cons (list current-section current-section-list) section-list))
        (t section-list))
      (let* ((line (car lines))
             (linelen (length line)))
        (cond
          ((= linelen 0)
           (sections (cdr lines) section-list current-section current-section-list))
          ((and (equal #\[ (char line 0)) (equal #\] (char line (1- linelen))))
           (sections (cdr lines) (unless (= (length current-section) 0)
                                     (cons (list current-section current-section-list) section-list))
                     (subseq line 1 (1- linelen))))
          (t
           (sections (cdr lines) section-list current-section (append current-section-list (list line))))))))

;; Get an ordered list of properties associated with [name] of a section
(defun get-section (section-name sections)
  (if (not sections)
      nil
      (let* ((section (car sections))
             (current-section-name (car section))
             (props (cadr section)))
        (if (equal current-section-name section-name)
            props
          (get-section section-name (cdr sections)))))) 

;; Go line by line in section until first element is property
(defun get-property (properties property)
  (if (not properties)
      nil
      (let* ((prop-s (car properties))
             (name-val (cl-ppcre:register-groups-bind (prop-name val)
                                            ("^:(\\w*) (.*)$" prop-s)
                         (list prop-name val)))
             (name (car name-val))
             (val (cadr name-val)))
        (if (equal property name)
            val
          (get-property (cdr properties) property)))))

;; Creates the category object from [category] section
(defun create-category-object (category-section)
  (make-instance 'category
                   :name (get-property category-section "name")
                   :percentage (get-property category-section "percentage")))

;; Creates the splits
(defun create-category-split-objects (category splits-section &optional (splits '()))
  (if (not splits-section)
      splits
      (create-category-split-objects
       category
       (cdr splits-section)
       (append
        splits
        (list (make-instance 'category-split
                               :name (get-property splits-section "name")
                               :category category))))))

;; Driver that takes the config and inserts the category and its 
;; splits into the db, obviously requires a mito toplevel connection
(defun import-config (file-path)
  (let*
      ((config-sections (sections (read-lines file-path)))
       (category (mito:insert-dao (create-category-object (get-section "category" config-sections))))
       (splits (mapcar 'mito:insert-dao (create-category-split-objects category (get-section "splits" config-sections)))))
    (list category splits)))
