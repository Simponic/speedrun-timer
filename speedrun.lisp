(defclass speedrun ()
  ((state
    ;; RUNNING, STOPPED
    :initarg :state
    :accessor speedrun-state)
   (title
    :initarg :title
    :accessor speedrun-title)
   ;; Whatever internal time units decided by SBCL (get-internal-real-time)
   ;; (local-time:now) *could* be used, but by my testing it's around 6 times slower
   ;; so why not
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
      (setf (speedrun-elapsed speedrun) (* 1000 (/ (- (get-internal-real-time) (speedrun-start-timestamp speedrun)) internal-time-units-per-second)))))

;; Initializes a speedrun to start running the timer
(defun start-speedrun (speedrun)
  (setf (speedrun-state speedrun) 'RUNNING
        (speedrun-start-timestamp speedrun) (get-internal-real-time)
        (run-split-start-time (current-split speedrun)) (local-time:now)))

;; Set the state of the speedrun to be stopped if there are no more splits.
;; Or, set the current split to the next one in the list.
(defun next-split (speedrun)
  (let ((now (local-time:now)))
    (setf (run-split-end-time (current-split speedrun)) now)
    (inc (speedrun-current-split-index speedrun))
    (if (equal (speedrun-current-split-index speedrun) (length (speedrun-splits speedrun)))
        (setf (speedrun-state speedrun) 'STOPPED)
      (setf (run-split-start-time (current-split speedrun)) now))))

;; Saves the speedrun into the database
(defun save-speedrun (speedrun)
  (mapcar #'mito:save-dao (cons (speedrun-run-dao speedrun) (speedrun-splits speedrun))))
