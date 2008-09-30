(in-package #:tinaa)

(defun parts-matching-symbol (symbol-part name-holder)
  "Returns a list of parts in name-holder than have the same name as symbol"
  (let ((value nil))
    (collect-elements 
     (subparts name-holder)
     :filter (lambda (subparts)
               (let ((matching
                      (search-for-item subparts (part-name symbol-part)
                                       :key #'part-name)))
                 (if (or (eq symbol-part matching)
                         (not (part-can-have-documention-p matching)))
                   (setf value nil)
                   (setf value matching))))
     :transform (lambda (x) (declare (ignore x)) value))))


(defun build-permuted-index (main-part parts-to-index heading)
  "A permuted index includes each n-word entry up to n times, at points corresponding to the use of each word in the entry as the sort key. For example, a symbol FOO-BAR would occur twice, once under FOO and BAR. This allows you to use any word in the symbol's name to search for that symbol."
  (declare (ignore heading))
  
  (bind ((writer (page-writer (root-parent main-part)))
         (parts (loop for symbol-part in parts-to-index nconc
                      (parts-matching-symbol symbol-part main-part)))
         (symbol-list (sort
                       (delete-duplicates
                        (append *required-index-contents*
                                (collect-elements 
                                 parts-to-index 
                                 :transform 
                                 (lambda (part) 
                                   (string-upcase (subseq (part-name part) 0 1)))))
                        :test #'string-equal)
                       #'string-lessp))
         (current-letter nil))
    (when (plusp (size parts))
      (bind ((parts-to-index
              (sort 
               (loop for part in parts 
                     when (index-part-p part) append
                     `((,part 0)
                       ,@(loop for index in (permuted-index-locations 
                                             (name part) 'permuted-index-delimiter-p) collect
                               (list part index))))
               #'string-lessp
               :key (lambda (part-index)
                      (bind (((part index) part-index))
                        (subseq (symbol-name (name part)) index)))))
             (max-size (reduce-elements
                        parts-to-index
                        #'max
                        :key (lambda (part.index)
                               (bind (((part index) part.index))
                                 (- (size (symbol-name (name part))) index))))))
        (documenting-page (main-part :title (format nil "Permuted Index of ~A"
                                                    (part-name main-part))
                                     :force-contents-link? t)
          ((:a :name "top")
           (:h3 "Permuted Index of " (lml-princ (part-name main-part))))
          (build-index-letters symbol-list parts-to-index :key #'car)
          
          ((:div :class "index-contents")
           (iterate-container
            parts-to-index
            (lambda (part.index)
              (bind (((part index) part.index)
                     (part-name (part-name part))
                     (ch (aref part-name index)))
                (unless (and current-letter (char-equal current-letter ch))
                  (setf current-letter ch)
                  (html ((:div :class "index-letter")
                         ((:a :name (format nil "~C-section" current-letter))
                          ((:div :class "the-letter") (lml-princ current-letter))
                          ((:div :class "back-to-top") 
                           ((:a :href "#top") "Back to top"))))))
                (display-part writer part :permuted-index :spaces (- max-size index)))))))))))

#+Testing
;;?? trying to use this iterator to not all the <pre></pre> stuff
(let ((part-iterator (make-iterator parts-to-index))
      (current-letter nil))
  (loop while (move-forward-p part-iterator) do
        (when (current-element-p part-iterator)
          (bind (((part index) (current-element part-iterator))
                 (part-name (part-name part))
                 (ch (aref part-name index)))
            (unless (and current-letter (char-equal current-letter ch))
              (setf current-letter ch)
              (html ((:div :class "index-letter")
                     ((:a :name (format nil "~C-section" current-letter))
                      ((:div :class "the-letter") (lml-princ current-letter))
                      ((:div :class "back-to-top") 
                       ((:a :href "#top") "Back to top"))))))
            (display-part writer part :permuted-index :spaces (- max-size index))))
        (move-forward part-iterator)))


(defun make-string-of-size (size initial-element)
  (coerce (make-array size :initial-element initial-element) 'string))


(defun princ-n-spaces (stream n)
  (loop repeat n do (princ " " stream)))


(defmethod display-part ((writer simple-page-writer) (part basic-doclisp-part)
                         (mode (eql :permuted-index)) &key (spaces 0) &allow-other-keys)
  (html
   (:pre (lml-princ (make-string-of-size spaces #\ )) 
         (display-part-for-index writer part (part-name part))
         (lml-format " \(~(~A~)\)" (part-kind-abbreviation part)))))


(defmethod part-kind-abbreviation (part)
  (part-kind part))
  

(defun permuted-index-delimiter-p (ch)
  (char-equal ch #\-))


(defun permuted-index-locations (symbol delimiter-p)
  (bind ((name (symbol-name symbol))
	 (largest-possible-delimiter-location (1- (size name))))
    (loop for x across name
       for i = 0 then (1+ i) 
       when (and (< i largest-possible-delimiter-location)
		 (funcall delimiter-p x)) collect (1+ i))))
