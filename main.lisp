(ql:quickload '(unix-opts mito cl-ppcre croatoan local-time))

;; Migrate database structure
(mito:connect-toplevel :sqlite3 :database-name #P"timer.db")
(setq mito:*auto-migration-mode* t)
(load "database/category.lisp")
(load "database/run.lisp")

;; Utils
(load "util.lisp")

;; Config file importing
(load "config.lisp")

;; Load the UI
(load "ui.lisp")

;; The timing logic
(load "speedrun.lisp")

;; Define command line arguments
(opts:define-opts
  (:name :import
   :description "create splits and category from a config file"
   :short #\i
   :long "import"
   :arg-parser #'identity))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))
(defun main ()
  (let ((options (opts:get-opts)))
    (when-option (options :import)
                 (import-config (getf options :import))))
  (run-ui))

(main)
