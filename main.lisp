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

(defun get-input (prompt &optional (validator 'nonempty-p))
  (clear-input)
  (write-string prompt)
  (finish-output)
  (let ((input (read-line)))
    (if (ignore-errors (funcall validator input))
        input
        (progn
          (format t "E: Invalid input. Try again.~%")
          (get-input prompt validator)))))

;; Options is an alist with the prompt string as the car and the value as the cdr
(defun select-option (options)
  (let ((i 0))
    (loop for x in options
          do
             (inc i)
             (format t "  [~a] ~a~%" i (car x))))
  (let ((user-input (get-input (format nil "Select [~a - ~a] or search: " 1 (length options)))))
    (if (every #'digit-char-p user-input)
        ;; Selected by option index
        (let ((user-integer (parse-integer user-input)))
          (if (and (>= user-integer 1) (<= user-integer (length options)))
              (cdr (nth (1- user-integer) options))
              (progn
                (format t "E: Not a valid selection.~%")
                (select-option options))))
        ;; Search for user string, either select the one it matches or recursively call select-option on the matched options
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
                 (format t "That search came up with multiple results:~%")
                 (select-option filtered)))
              (progn (format t "E: Could not find option that matched query.~%")
                     (select-option options)))))))

(defun user-create-new-category ()
  (let* ((name (get-input "Category Name (e.g. \"SM64\"): "))
         (percentage (get-input "Percentage (e.g. \"Any% 16 Star\"): "))
         (category (mito:insert-dao (make-instance 'category :name name :percentage percentage)))
         (splits (do ((spliti 1 (1+ spliti))
                      (inputs '() (push (get-input (format nil "Split Name [~a]~a: " spliti (if (<= spliti 1) " (blank when done adding)" "")) (lambda (x) t)) inputs)))
                     ((equal (car inputs) "")
                      (mapcar (lambda
                                  (category-split-name)
                                (mito:insert-dao
                                 (make-instance 'category-split
                                                :name category-split-name
                                                :category category)))
                              (reverse (cdr inputs)))))))
    (if splits
        (format t "Successfully created category~%"))))

(defun with-selected-category (f)
  (let* ((categories (mito:select-dao 'category))
         (category-alist (mapcar (lambda (category) `(,(format nil "~a - ~a" (category-name category) (category-percentage category)) . ,category)) categories)))
    (if categories
        (funcall f (select-option category-alist))
        (format t "E: There are no categories. Try creating one or importing one~%"))))

(defun with-selected-speedrun (f)
  (let* ((filter (select-option '(("Choose from a category" . CATEGORY) ("List runs from all categories" . ALL))))
         (runs
          (case filter
            ('CATEGORY (with-selected-category 'list-category-runs))
            ('ALL (list-runs))))
         (run-details-alist (mapcar (lambda (run-detail)
                                      `(,(let ((formatted-elapsed (format-time (make-time-alist (getf run-detail :ELAPSED))))
                                               (category-name (getf run-detail :NAME))
                                               (category-percentage (getf run-detail :PERCENTAGE)))
                                           (apply 'format
                                                  (if (and category-name category-percentage)
                                                      `(nil "~a - ~a | ~a" ,category-name ,category-percentage ,formatted-elapsed)
                                                      `(nil "~a" ,formatted-elapsed))))
                                         . ,(mito:find-dao 'run :id (getf run-detail :RUN-ID))))
                                    runs)))
    (if run-details-alist
        (funcall f (select-option run-details-alist))
        (progn
          (format t "E: No runs found~%")
          (if (y-or-n-p "Go back?")
              nil
              (with-selected-speedrun f))))))

(defun main ()
  (let ((choice (select-option '(("Help" . HELP)
                                 ("Import a category" . IMPORT-CATEGORY)
                                 ("Make a new category" . NEW-CATEGORY)
                                 ("Delete a category" . DELETE-CATEGORY)
                                 ("Start a speedrun" . START-SPEEDRUN)
                                 ("View splits of a speedrun" . VIEW-SPEEDRUNS)
                                 ("Delete a speedrun" . DELETE-SPEEDRUN)
                                 ("Exit" . EXIT)))))
    (case choice
      ('HELP
       (format t "~%")
       (mapcar #'(lambda (x) (format t "~a~%" x)) *lispruns-logo*)
       (format t "Welcome to Lispruns!~%"))
      ('IMPORT-CATEGORY
       (if (import-category (get-input
                         (format nil "Relative or absolute path to configuration file [~a]: "
                                 (uiop/os:getcwd))
                         'probe-file))
           (format t "Successfully imported category~%")))
      ('NEW-CATEGORY
       (user-create-new-category))
      ('START-SPEEDRUN
       (with-selected-category 'speedrun-ui))
      ('DELETE-SPEEDRUN
       (with-selected-speedrun 'mito:delete-dao))
      ('DELETE-CATEGORY
       (with-selected-category (lambda (category)
                                 (let ((runs
                                         (mapcar
                                          (lambda (run-detail) (mito:find-dao 'run :id (getf run-detail :RUN-ID)))
                                          (list-category-runs category))))
                                   (mapcar 'delete-run runs))
                                 (mito:delete-dao category)))
       (format t "Deleted category~%"))
      ('VIEW-SPEEDRUNS
       (with-selected-speedrun (lambda (run)
                                 (let ((csplits (category-splits (run-category run)))
                                       (rsplits (run-splits run)))
                                   (mapcar (lambda (csplit rsplit)
                                             (format t "  ~a~%" (format-line `((,(category-split-name csplit) . ,(/ 3 10))
                                                                      ("|" . ,(/ 1 10))
                                                                      (,(run-split-format-elapsed-time rsplit) . ,(/ 6 10)))
                                                                    70 0)))
                                           csplits rsplits)))))
      ('EXIT
       (quit))))
  (format t "~%")
  (main))
