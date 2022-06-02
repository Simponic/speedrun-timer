(mito:deftable run ()
  ((category :col-type category))
  (:record-timestamps nil)
  (:conc-name run-))
  
(mito:deftable run-split ()
  ((run :col-type run)
   (category-split :col-type category-split)
   (start-time :col-type (or :datetime :null))
   (end-time :col-type (or :datetime :null)))
  (:record-timestamps nil)
  (:conc-name run-split-))



(defun run-splits (run)
  (mito:select-dao 'run-split
                   (sxql:order-by :category_split_id)
                   (sxql:where (:= :run run))))

;; Returns the elapsed time in milliseconds since split started to either
;; current time or the split's end time
(defun run-split-elapsed-time (run-split)
  (let ((start (ignore-errors (run-split-start-time run-split)))
        (end (or (ignore-errors (run-split-end-time run-split)) (local-time:now))))
    (if start
        (floor (* 100 (local-time:timestamp-difference end start))))))

(defun run-split-format-elapsed-time (run-split)
  (let ((elapsed (run-split-elapsed-time run-split)))
    (if elapsed
        (format-time (make-time-alist elapsed))
        "-")))



(defmacro query-with-runs-elapsed (&rest body)
  `(mito:retrieve-by-sql
    (sxql:select (:* (:as (:raw "sum(julianday(end_time) - julianday(start_time))*24*60*60") :elapsed))
                 (sxql:from :run_split)
                 (sxql:group-by :run_id)
                 ,@body)))

(defun best-category-run (category)
  (query-with-runs-elapsed
   (sxql:inner-join :run :on (:= :run_id :run.id))
   (sxql:order-by :elapsed)
   (sxql:limit 1)
   (sxql:where (:= :category_id (mito:object-id category)))))

(defun best-category-split (category-split)
  (query-with-runs-elapsed
   (sxql:inner-join :category_split :on (:= :category_split_id :category_split.id))
   (sxql:order-by :elapsed)
   (sxql:limit 1)
   (sxql:where (:= :category_split_id (mito:object-id category-split)))))

(defun list-runs (&key (order-element :end-time) (direction :asc))
  (query-with-runs-elapsed
   (sxql:inner-join :run :on (:= :run_id :run.id))
   (sxql:inner-join :category :on (:= :category_id :category.id))
   (sxql:order-by (list direction order-element))))

(defun list-category-runs (category &key (order-element :elapsed) (direction :asc))
  (query-with-runs-elapsed
   (sxql:inner-join :run :on (:= :run_id :run.id))
   (sxql:order-by (list direction order-element))
   (sxql:where (:= :category_id (mito:object-id category)))))
