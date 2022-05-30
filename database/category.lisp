(mito:deftable category ()
  ((name :col-type (:varchar 128))
   (percentage :col-type (:varchar 128)))
  (:record-timestamps nil)
  (:conc-name category-))

(mito:deftable category-split ()
  ((name :col-type (:varchar 128))
   (category :col-type category))
  (:record-timestamps nil)
  (:conc-name category-split-))

(defun category-splits (category)
  (mito:select-dao 'category-split
                   (sxql:where (:= :category category))
                   ;; Assumption that split categories are entered in the correct order by id
                   (sxql:order-by :id)))
