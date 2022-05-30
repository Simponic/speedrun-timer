(defparameter *lispruns-logo* 
  '("db      d888888b .d8888. d8888b. d8888b. db    db d8b   db .d8888."
    "88        `88'   88'  YP 88  `8D 88  `8D 88    88 888o  88 88'  YP"
    "88         88    `8bo.   88oodD' 88oobY' 88    88 88V8o 88 `8bo.  "   
    "88         88      `Y8b. 88~~~   88`8b   88    88 88 V8o88   `Y8b."
    "88booo.   .88.   db   8D 88      88 `88. 88b  d88 88  V888 db   8D"
    "Y88888P Y888888P `8888Y' 88      88   YD ~Y8888P' VP   V8P `8888Y'"))

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

(defun run-ui (category)
  (croatoan:with-screen (scr :input-blocking nil :input-echoing nil :cursor-visible nil :enable-colors t :input-buffering nil :input-blocking nil)
    (setf (croatoan:background scr) (make-instance 'croatoan:complex-char :color-pair (cdr (assoc 'main *colors*))))
    (let* ((state 'TITLE)
           (redraws '(title-instance))
           (speedrun (make-speedrun category)))
      (croatoan:event-case (scr event)
        (#\q (return-from croatoan:event-case))
        (#\space
         (case state
           ('TITLE
            (start-speedrun speedrun)
            (setf state 'RUNNING))
           ('RUNNING (next-split speedrun))))
        (:resize nil)
        ((nil)
         (case state
           ('TITLE 
            (if (member 'title-instance redraws)
                (let* ((padding 3)
                       (width (+ (* 2 padding) (max-length *lispruns-logo*)))
                       (height (+ (* 2 padding) (length *lispruns-logo*)))
                       (logo-centered (center-box scr width height))
                       (logo-box (make-instance 'croatoan:window :border t :width width :height height :position logo-centered)))
                  (write-horizontal-slice-list logo-box `(,padding ,padding) *lispruns-logo*)
                  (croatoan:refresh logo-box))))
            ('RUNNING
             (update-time speedrun)
             (let ((timer-instance (timer-window speedrun '(10 10) 70 10)))
               (croatoan:refresh timer-instance))))
         (setf redraws '())
         (sleep (/ 1 30)))))))


;;    (setq hl (make-instance 'highlight-list
;;                            :scroll-i 0
;;                            :elements `(
;;                                        (("HELLO" . ,(/ 1 2)) ("" . ,(/ 1 2)))
;;                                        (("THIS IS A TEST" . ,(/ 1 2)) (" OF WRAPPING TRUNCATION" . ,(/ 1 2)))
;;                                        )
;;                            :current-element-index current-index
;;                            :height 6
;;                            :width 20))
