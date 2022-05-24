(defparameter *lispruns-logo* 
  '("db      d888888b .d8888. d8888b. d8888b. db    db d8b   db .d8888."
    "88        `88'   88'  YP 88  `8D 88  `8D 88    88 888o  88 88'  YP"
    "88         88    `8bo.   88oodD' 88oobY' 88    88 88V8o 88 `8bo.  "   
    "88         88      `Y8b. 88~~~   88`8b   88    88 88 V8o88   `Y8b."
    "88booo.   .88.   db   8D 88      88 `88. 88b  d88 88  V888 db   8D"
    "Y88888P Y888888P `8888Y' 88      88   YD ~Y8888P' VP   V8P `8888Y'"))

(defparameter *colors*
  '((main . (:green :black))
    (figlet . (:black :white))
    (selected-highlight . (:blue :black))
    (unselected-highlight . (:white :black))))

;; Returns (y, x) to draw box to center it in screen
(defun center-box (screen width height)
  (let ((sw (croatoan:width screen))
        (sh (croatoan:height screen)))
    (list (round (- (/ sh 2) (/ height 2))) (round (- (/ sw 2) (/ width 2))))))

(defun write-horizontal-slice-list (scr pos slices)
  (let ((yi (car pos)))
    (mapcar (lambda (s)
              (croatoan:add scr s :position `(,yi ,(cadr pos)))
              (inc yi))
            slices)))

;; Draws a list of strings horizontally in a window with padding and an optional border
(defun figlet-window (title-slices scr pos &key (padding 2) (border nil))
  (let* ((width (+ (reduce (lambda (a x) (max a x)) (mapcar #'length title-slices)) (* 2 padding)))
         (height (+ (length *lispruns-logo*) (* 2 padding)))
         (title-box (make-instance 'croatoan:window
                                   :border border
                                   :width width
                                   :height height
                                   :position pos)))
    (setf (croatoan:background title-box) (make-instance 'croatoan:complex-char :color-pair (cdr (assoc 'figlet *colors*))))
    (write-horizontal-slice-list title-box `(,padding ,padding) title-slices)
    title-box))

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

(defun highlight-list-window (hl pos)
  (let* ((width (- (highlight-list-width hl) 2))
         (height (- (highlight-list-height hl) 2))
         (elements (highlight-list-elements hl))
         (current-element-index (highlight-list-current-element-index hl))
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
                                        :width (+ 2 width)
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
         
(defun run-ui ()
  (croatoan:with-screen (scr :input-blocking nil :input-echoing nil :cursor-visible nil :enable-colors t :input-buffering nil :input-blocking nil)
    (croatoan:clear scr)
    (croatoan:refresh scr)
    (setf (croatoan:background scr) (make-instance 'croatoan:complex-char :color-pair (cdr (assoc 'main *colors*))))
    (croatoan:draw-border scr)

    (defvar windows '())
    (defvar current-index 0)
    (croatoan:event-case (scr event)
                         (#\b
                          (let ((hl (make-instance 'highlight-list
                                                   :scroll-i 0
                                                   :elements `(
                                                               (("HELLO" . ,(/ 1 2)) ("" . ,(/ 1 2)))
                                                               (("THIS IS A TEST" . ,(/ 1 2)) (" OF WRAPPING TRUNCATION" . ,(/ 1 2)))
                                                              )
                                                   :current-element-index current-index
                                                   :height 6
                                                   :width 20)))
                            (push (highlight-list-window hl '(10 20)) windows))
                          (push (figlet-window *lispruns-logo* scr '(2 2)) windows)
                          (inc current-index))
                         (#\q (return-from croatoan:event-case))
                         (#\c (croatoan:clear scr))
                         (:resize nil)
                         ((nil)
                          (mapcar #'croatoan:refresh (cons scr windows))
                          (inc current-scroll-index 0.02)
                          (sleep (/ 1 60))))))
