(in-package #:tinaa)

(defparameter *tinaa-home-page* "http://common-lisp.net/project/tinaa/")
(defparameter *tinaa-version* "0.3")
(defvar *document-stream* *standard-output*)
(defvar *document-file* nil)
(defvar *document-root* nil)
(defvar *root-part* nil)
(defvar *current-index* nil)
(defvar *current-part-index* 0)
(defvar *current-part* nil) 
(defparameter *short-documentation-length* 100
  "The number of characters of documentation to show in summaries.")
(defvar *packages-to-document* nil)
(defvar *default-packages-to-ignore* nil) 
(defvar *output-calls* nil)
(defvar *css-file* nil
  "If set, this should point to the CSS file that Tinaa should use.")
