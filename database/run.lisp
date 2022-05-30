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

;; Returns stuff like PB, best of each split, etc.
(defun run-statistics (category)
  `((asdf . 1)))

;; select *, sum(julianday(end_time)-julianday(start_time))*24*60*60 as total_time from run_split group by run_id order by total_time;
