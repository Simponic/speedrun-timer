;; Pads string 's' on the right by 'width'
(defun pad-right-string (s width)
  (format nil (concatenate 'string "~" (write-to-string width) "a") s))

;; Wraps text and adds a hypen if it doesn't fit within (1- width), scrolling
;; by index i
(defun maybe-wrap-text (text width i)
  (let ((textlen (length text)))
    (if (>= width textlen)
        text
        (let* ((max-width (1- width))
               (max-wrap (1+ (- textlen max-width)))
               (wrap-i (rem i max-wrap)))
          (concatenate 'string (subseq text wrap-i (+ wrap-i (min max-width textlen))) "-")))))

;; line is an alist containing the string as the first element and the
;; fraction of the maximum width "max-width" the whole line should take up (these should
;; add up to 1)
;; scroll-i is the index the string is truncated to with a hyphen (see maybe-wrap-text)
;; ex. (format-line `(("Hello, world" . ,(/ 2 5))
;;                    ("" . ,(/ 1 5))
;;                    ("Hello, again" . ,(/ 2 5)))
;;                  20 2)
;; -> "llo, wo-    llo, ag-"
(defun format-line (line max-width &optional (scroll-i 0) (formatted ""))
  (if (eq line nil)
      formatted
      (if (listp line)
          (let* ((curr (car line))
                 (text-width (floor (* max-width (cdr curr))))
                 (wrapped-string (maybe-wrap-text (car curr) text-width scroll-i))
                 (current-string (pad-right-string wrapped-string text-width)))
            (format-line (cdr line) max-width scroll-i (concatenate 'string formatted current-string)))
          (pad-right-string (maybe-wrap-text line max-width scroll-i) max-width))))

;; Add a list of strings representing horizontal slices of a character to another list of strings representing horizontal slices of a string, or create a new list of horizontal slices if the original was empty.
;; Character height will be truncated to the height of the first character in the slices.
(defun add-to-horizontal (character horizontal-layers &key (seperator " "))
  (let ((layer-height (length horizontal-layers)))
    (loop for i from 0 to (1- (if (zerop layer-height) (length character) layer-height))
          collect
          (let ((layer (nth i horizontal-layers))
                (character-slice (nth i character)))
            (if (and layer (> (length layer) 0))
                (concatenate 'string layer seperator character-slice)
              character-slice)))))

;; Creates a list of horizontal slices to display a formatted larger string by using figlet characters
(defun lispglet (str &optional (char-hashes *big-digits*))
  (loop for horizontal-layers = '()
        then (add-to-horizontal (gethash c char-hashes) horizontal-layers)
        for c across str
        finally (return horizontal-layers)))
