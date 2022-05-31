;; Migrate database structure
(mito:connect-toplevel :sqlite3 :database-name #P"timer.db")
(setq mito:*auto-migration-mode* t)
(load "database/category.lisp")
(load "database/run.lisp")

(defparameter *lispruns-logo* 
  '("db      d888888b .d8888. d8888b. d8888b. db    db d8b   db .d8888."
    "88        `88'   88'  YP 88  `8D 88  `8D 88    88 888o  88 88'  YP"
    "88         88    `8bo.   88oodD' 88oobY' 88    88 88V8o 88 `8bo.  "   
    "88         88      `Y8b. 88~~~   88`8b   88    88 88 V8o88   `Y8b."
    "88booo.   .88.   db   8D 88      88 `88. 88b  d88 88  V888 db   8D"
    "Y88888P Y888888P `8888Y' 88      88   YD ~Y8888P' VP   V8P `8888Y'"))

(defun get-input (prompt &optional (validator (lambda (x) t)))
  (clear-input)
  (write-string prompt)
  (finish-output)
  (let ((input (read-line)))
    (if (ignore-errors (funcall validator input))
        input
        (get-input prompt validator))))

;; Options is an alist with the prompt string as the car and the value as the cdr
(defun select-option (options)
  (let ((i 0))
    (loop for x in options
          do
             (inc i)
             (format t "  [~a] ~a~%" i (car x))))
  (let ((user-input (get-input (format nil "Select [~a - ~a] or search: " 1 (length options)))))
    (if (every #'digit-char-p user-input)
        (let ((user-integer (parse-integer user-input)))
          (if (and (>= user-integer 1) (<= user-integer (length options)))
              (cdr (nth (1- user-integer) options))
              (select-option options)))
        (let* ((scanner (cl-ppcre:create-scanner user-input :case-insensitive-mode t))
               (filtered
                 (remove-if-not
                  (lambda (option) (cl-ppcre:scan scanner (car option)))
                  options)))
          (if filtered
              (case (length filtered)
                (1 (let ((searched (car filtered)))
                     (if (y-or-n-p "Use \"~a\"" (car searched))
                         (cdr searched)
                         (select-option options))))
                (t
                 (format t "That search came up with multiple results:")
                 (select-option filtered)))
              (progn (format t "E: Could not find option that matched query.~%")
                     (select-option options))))))
  (format t "~%"))

(defun main ()
  (let ((choice (select-option '(("Help" . HELP)
                                 ("Import a category" . IMPORT-CATEGORY)
                                 ("Make a new category" . NEW-CATEGORY)
                                 ("Start a speedrun" . START-SPEEDRUN)
                                 ("Statistics" . LIST-CATEGORIES)
                                 ("Exit" . EXIT)))))
    (format t "~a~%" choice)
    (case choice
      ('HELP
       (mapcar #'(lambda (x) (format t "~a~%" x)) *lispruns-logo*))
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
