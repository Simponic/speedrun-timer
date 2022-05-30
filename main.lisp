;; Migrate database structure
(mito:connect-toplevel :sqlite3 :database-name #P"timer.db")
(setq mito:*auto-migration-mode* t)

;; Define command line arguments
(opts:define-opts
  (:name :import
   :description "create splits and category from a config file"
   :short #\i
   :long "import"
   :arg-parser #'identity))

(defun main ()
  (let ((options (opts:get-opts)))
    (when-option (options :import)
      (import-config (getf options :import)))
    (run-ui (car (mito:select-dao 'category)))))
