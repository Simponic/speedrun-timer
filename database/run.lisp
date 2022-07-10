(mito:deftable run ()
  ((category :col-type category :accessor run-category))
  (:record-timestamps nil)
  (:conc-name run-))
  
(mito:deftable run-split ()
  ((run :col-type run :accessor run-split-run)
   (category-split :col-type category-split :accessor run-split-category-split)
   (start-timestamp :col-type (or :bigint :null))
   (end-timestamp :col-type (or :bigint :null)))
  (:record-timestamps nil)
  (:conc-name run-split-))


(defun run-splits (run)
  (mito:select-dao 'run-split
                   (sxql:order-by :category_split_id)
                   (sxql:where (:= :run run))))

(defun delete-run (run)
  (let ((splits (run-splits run)))
    (mapcar 'mito:delete-dao (cons run splits))))

;; Returns the elapsed time in milliseconds since split started to either
;; current time or the split's end time
(defun run-split-elapsed-time (run-split)
  (let ((start (ignore-errors (run-split-start-timestamp run-split)))
        (end (or (ignore-errors (run-split-end-timestamp run-split)) (get-internal-real-time))))
    (if start
        (- end start))))

(defun run-split-format-elapsed-time (run-split)
  (let ((elapsed (run-split-elapsed-time run-split)))
    (if elapsed
        (format-time (make-time-alist (millis-since-internal-timestamp 0 elapsed)))
        "-")))

(defmacro query-with-runs-elapsed (&rest body)
  `(mito:retrieve-by-sql
    (sxql:select (:* (:as (:sum (:* (:/ (:raw "end_timestamp - CAST(start_timestamp AS REAL)") ,internal-time-units-per-second) 1000)) :elapsed))
                 (sxql:from :run_split)
                 (sxql:group-by :run_id)
                 ,@body)))

(defun best-category-run (category)
  (car (query-with-runs-elapsed
        (sxql:inner-join :run :on (:= :run_id :run.id))
        (sxql:order-by :elapsed)
        (sxql:limit 1)
        (sxql:where (:= :category_id (mito:object-id category))))))

(defun best-category-split (category-split)
  (car (query-with-runs-elapsed
        (sxql:inner-join :category_split :on (:= :category_split_id :category_split.id))
        (sxql:order-by :elapsed)
        (sxql:limit 1)
        (sxql:where (:= :category_split_id (mito:object-id category-split))))))

(defun list-runs (&key (order-element :id) (direction :asc))
  (query-with-runs-elapsed
   (sxql:inner-join :run :on (:= :run_id :run.id))
   (sxql:inner-join :category :on (:= :category_id :category.id))
   (sxql:order-by (list direction order-element))))

(defun list-category-runs (category &key (order-element :elapsed) (direction :asc))
  (query-with-runs-elapsed
   (sxql:inner-join :run :on (:= :run_id :run.id))
   (sxql:order-by (list direction order-element))
   (sxql:where (:= :category_id (mito:object-id category)))))


(defun statistics (category)
  (let ((csplits (category-splits category)))
    `((SPLIT-PBS . ,(mapcar (lambda (csplit) (getf (best-category-split csplit) :ELAPSED)) csplits))
      (BEST-CATEGORY-RUN-SPLITS . ,(or
                                  (mapcar (lambda (split)
                                            (millis-since-internal-timestamp 0 (run-split-elapsed-time split)))
                                          (ignore-errors
                                           (run-splits (mito:find-dao 'run :id (getf (best-category-run category) :RUN-ID)))))
                                  (mapcar (lambda (csplit) nil) csplits))))))
  
