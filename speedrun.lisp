(defclass speedrun ()
  ((state
    ;; RUNNING, STOPPED
    :initarg :state
    :accessor speedrun-state)
   (title
    :initarg :title
    :accessor speedrun-title)
   (start-timestamp
    :initarg :start-timestamp
    :accessor speedrun-start-timestamp)
   (elapsed ;; milliseconds
    :initarg :elapsed
    :accessor speedrun-elapsed)
   (splits
    :initarg :splits
    :accessor speedrun-splits)
   (run-dao
    :initarg :run-dao
    :accessor speedrun-run-dao)
   (current-split-index
    :initarg :current-split-index
    :accessor speedrun-current-split-index)))

(defun make-speedrun (category)
  (let* ((run (make-instance 'run :category category))
         (splits (mapcar (lambda (category-split)
                           (make-instance 'run-split :category-split category-split :run run))
                         (category-splits category))))
    (make-instance 'speedrun
                   :state 'STOPPED
                   :title (category-name category)
                   :splits splits
                   :current-split-index 0
                   :elapsed 0.0
                   :run-dao run)))

(defun current-split (speedrun)
  (nth (speedrun-current-split-index speedrun) (speedrun-splits speedrun)))

;; Updates the current total elapsed time of the speedrun if it's running
(defun update-time (speedrun)
  (if (eq (speedrun-state speedrun) 'RUNNING)
      (setf (speedrun-elapsed speedrun) (millis-since-internal-timestamp (speedrun-start-timestamp speedrun)))))

;; Initializes a speedrun to start running the timer
(defun start-speedrun (speedrun)
  (let ((now (get-internal-real-time)))
    (setf (speedrun-state speedrun) 'RUNNING
          (speedrun-start-timestamp speedrun) now 
          (run-split-start-timestamp (current-split speedrun)) now)))

;; Saves the speedrun into the database
(defun save-speedrun (speedrun)
  (mapcar #'mito:save-dao (cons (speedrun-run-dao speedrun) (speedrun-splits speedrun))))

;; Set the state of the speedrun to be stopped if there are no more splits.
;; Or, set the current split to the next one in the list.
(defun next-split (speedrun)
  (let ((now (get-internal-real-time)))
    (unless (equal (speedrun-state speedrun) 'STOPPED)
      (setf (run-split-end-timestamp (current-split speedrun)) now)
      (if (equal (speedrun-current-split-index speedrun) (1- (length (speedrun-splits speedrun))))
          (progn
            (setf
             ;; Since timer computation can get +-0.02 seconds out of sync of splits, just set it to the sum of the splits' elapsed time
             (speedrun-elapsed speedrun) (millis-since-internal-timestamp 0 (apply '+ (mapcar 'run-split-elapsed-time (speedrun-splits speedrun)))) 
             (speedrun-state speedrun) 'STOPPED)
            (save-speedrun speedrun))
          (progn
            (inc (speedrun-current-split-index speedrun))
            (setf (run-split-start-timestamp (current-split speedrun)) now))))))

