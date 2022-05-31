;; Migrate database structure
(mito:connect-toplevel :sqlite3 :database-name #P"timer.db")
(setq mito:*auto-migration-mode* t)
(load "database/category.lisp")
(load "database/run.lisp")

(defun get-input (prompt validation)
  (clear-input)
  (write-string prompt)
  (finish-output)
  (let ((input (read-line)))
    (if (ignore-errors (funcall validation input))
        input
        (get-input prompt validation))))

;; Options is an alist with the prompt string as the car and the value as the cdr
(defun get-option (options)
  (let ((i 0))
    (loop for x in options
          do
             (inc i)
             (format t "  [~a] ~a~%" i (car x))))
  (cdr (nth (1- (parse-integer (get-input
                           (format nil "[~a - ~a]: " 1 (length options)) (lambda (x) (let ((user-integer (parse-integer x)))
                                         (and
                                          (>= user-integer 1)
                                          (<= user-integer (length options))))))))
       options)))

(defun main ()
  (mapcar #'(lambda (x) (format t "~a~%" x)) *lispruns-logo*)
  (let ((choice (get-option '(("Help" . HELP)
                              ("Import a category" . IMPORT-CATEGORY)
                              ("Make a new category" . NEW-CATEGORY)
                              ("Start a speedrun" . START-SPEEDRUN)
                              ("Statistics" . LIST-CATEGORIES)
                              ("Exit" . EXIT)))))
    (case choice
      ('IMPORT-CATEGORY
       (import-category (get-input (format nil "Relative or absolute path to configuration file [~a]: " (uiop/os:getcwd)) 'probe-file)))
      ('NEW-CATEGORY
       (format t "NEW CATEGORY~%"))
      ('START-SPEEDRUN
       (speedrun-ui (car (mito:select-dao 'category))))
      ('EXIT
       (quit))))
  (format t "~%")
  (main))
        
  
;;  (let ((options (opts:get-opts)))
;;    (when-option (options :import)
;;      (import-config (getf options :import)))
;;    (run-ui (car (mito:select-dao 'category)))))
