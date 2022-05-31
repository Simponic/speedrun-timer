(defparameter *colors*
  '((main . (:green :black))
    (timer-box . (:red :black))
    (selected-highlight . (:blue :black))
    (unselected-highlight . (:white :black))))

;; Returns (y, x) to draw box to center it in screen
(defun center-box (screen width height)
  (let ((sw (croatoan:width screen))
        (sh (croatoan:height screen)))
    (list (round (- (/ sh 2) (/ height 2))) (round (- (/ sw 2) (/ width 2))))))

;; Write a list of horizontal slices to the screen scr at position pos
(defun write-horizontal-slice-list (scr pos slices)
  (let ((yi (car pos)))
    (mapcar (lambda (s)
              (croatoan:add scr s :position `(,yi ,(cadr pos)))
              (inc yi))
            slices)))

;; Creates a window with the total time and statistics 
(defun timer-window (speedrun pos width height)
  (let* ((timerglet (lispglet (format-time (make-time-alist (speedrun-elapsed speedrun)))))
         (timer-box (make-instance 'croatoan:window
                                   :border t
                                   :position pos
                                   :width width 
                                   :height height)))
    (setf (croatoan:color-pair timer-box)
          (cdr (assoc 'timer-box *colors*)))
    (write-horizontal-slice-list timer-box '(1 1) timerglet)
    timer-box))

;; Class to hold state for a list where one element is highlighted/selected
(defclass highlight-list ()
  ((scroll-i
    :initarg :scroll-i
    :accessor highlight-list-scroll-i)
   (elements
    :initarg :elements
    :accessor highlight-list-elements)
   (current-element-index
    :initarg :current-element-index
    :accessor highlight-list-current-element-index)
   (height
    :initarg :height
    :accessor highlight-list-height)
   (width
    :initarg :width
    :accessor highlight-list-width)))

;; Create the actual window to render a highlight list hl at position pos
(defun highlight-list-window (hl pos)
  (let* ((width (- (highlight-list-width hl) 2)) ;; Magic number 2's are for the border on both sides
         (height (- (highlight-list-height hl) 2))
         (elements (highlight-list-elements hl))
         (current-element-index (mod (highlight-list-current-element-index hl) (length elements)))
         (elements-to-draw-subseq (if (>= height (length elements))
                                      (list 0 (length elements))
                                      (cond
                                        ((> height (1+ current-element-index))
                                         (list 0 height))
                                        ((< (- (length elements) height) current-element-index)
                                         (list (- (length elements) height) (length elements)))
                                        (t (let ((dy (/ (1- height) 2)))
                                             (list (- current-element-index (floor dy)) (1+ (+ current-element-index (ceiling dy)))))))))
         (highlight-menu (make-instance 'croatoan:window
                                        :border t
                                        :width (+ 2 width) ;; Another magic 2
                                        :height (+ 2 height)
                                        :position pos)))
   (let ((yi 0))
     (mapcar (lambda (el)
               (setf (croatoan:color-pair highlight-menu)
                     (if (equal (+ yi (car elements-to-draw-subseq)) current-element-index)
                         (cdr (assoc 'selected-highlight *colors*))
                         (cdr (assoc 'unselected-highlight *colors*))))
               (inc yi)
               (croatoan:add highlight-menu
                             (format-line el width (highlight-list-scroll-i hl))
                             :position `(,yi 1)))
             (subseq elements (car elements-to-draw-subseq) (cadr elements-to-draw-subseq))))
   highlight-menu))

(defun speedrun-ui (category)
  (croatoan:with-screen (scr :input-blocking nil :input-echoing nil :cursor-visible nil :enable-colors t :input-buffering nil :input-blocking nil)
    (setf (croatoan:background scr) (make-instance 'croatoan:complex-char :color-pair (cdr (assoc 'main *colors*))))
    (let* ((scroll 0)
           (frame 0)
           (state 'TITLE)
           (redraws '(title-instance))
           (speedrun (make-speedrun category))
           (csplits (category-splits category)))
      (flet ((render () 
               (case state
                 ('TITLE 
                  (if (member 'title-instance redraws)
                      (croatoan:clear scr)
                      (let* ((padding 4)
                             (title (append *lispruns-logo* '("" "CONTROLS" "  SPACE to start and to continue to the next split" "  Q to quit")))
                             (width (+ (* 2 padding) (max-length title)))
                             (height (+ (* 2 padding) (length title)))
                             (logo-centered (center-box scr width height))
                             (logo-box (make-instance 'croatoan:window :border t :width width :height height :position logo-centered)))
                        (if (< (croatoan:width scr) width)
                            (progn
                              (croatoan:add scr "Please increase width of your terminal" :position '(0 0))
                              (croatoan:refresh scr))
                            (progn
                              (write-horizontal-slice-list logo-box `(,padding ,padding) title)
                              (croatoan:refresh logo-box))))))
                 ('RUNNING
                  (if (eq (speedrun-state speedrun) 'RUNNING)
                      (update-time speedrun))
                  (if (member 'timer-instance redraws)
                      (croatoan:clear scr))
                  (if (zerop (mod frame 4)) 
                      (let* ((max-width (min 90 (croatoan:width scr)))
                             (centered-x (cadr (center-box scr max-width 0)))
                             (timer-height 8)
                             (splits-height (- (croatoan:height scr) timer-height))
                             (split-list (make-instance 'highlight-list
                                                        :scroll-i scroll 
                                                        :current-element-index (speedrun-current-split-index speedrun)
                                                        :height splits-height
                                                        :width max-width
                                                        :elements (mapcar (lambda (csplit speedrun-split)
                                                                            `(
                                                                              (,(category-split-name csplit) . ,(/ 4 12))
                                                                              ("" . ,(/ 1 12))
                                                                              (,(format-elapsed-time speedrun-split) . ,(/ 3 12))
                                                                              ))
                                                                          csplits
                                                                          (speedrun-splits speedrun))))
;;                                                        :elements (mapcar #'category-split-name csplits)))
;;                                                        :elements `((("FIRST SPLIT IS EPIC" . ,(/ 4 12)) ("" . ,(/ 1 12)) ("10:10:00.22" . ,(/ 3 12)) ("" . ,(/ 1 12)) ("20:00.00" . ,(/ 3 12))))))
                             (splits-instance (highlight-list-window split-list `(0 ,centered-x)))
                             (timer-instance (timer-window speedrun `(,splits-height ,centered-x) max-width timer-height)))
                        (croatoan:refresh splits-instance)
                        (croatoan:refresh timer-instance)))))
               (setf redraws '()
                     frame (mod (1+ frame) 60))
               (if (zerop (mod frame 30))
                   (inc scroll))
               (sleep (/ 1 60))))
        (croatoan:event-case (scr event)
                             (#\q (return-from croatoan:event-case))
                             (#\space
                              (case state
                                ('TITLE
                                 (start-speedrun speedrun)
                                 (setf redraws '(timer-instance))
                                 (setf state 'RUNNING))
                                ('RUNNING (next-split speedrun))))
                             (:resize
                              (case state
                                ('TITLE
                                 (setf redraws '(title-instance)))
                                ('RUNNING
                                 (croatoan:clear scr)))
                              (render))
                             ((nil) (render)))))))
