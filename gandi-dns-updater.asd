(defpackage gandi-dns-updater/system
  (:use
   :cl
   :asdf
   :uiop))

(in-package :gandi-dns-updater/system)

(defsystem "gandi-dns-updater"
  :description "script to update gandi-dns"
  :version "0.0.2"
  :author "Dan Miller"
  :depends-on (:drakma
	       :cl-json
	       :local-time
	       :alexandria
	       :split-sequence
	       :cl-stun)
  :serial t
  :components ((:file "package")
	       (:file "variables")
	       (:static-file "public-stun-list.txt")
	       (:file "gandi")))

(defun read-file-lines (file)
  (with-open-file (i file)
    (do (list) (nil)
      (let ((line (read-line i nil)))
	(if line (push line list)
	    (return list))))))

(defmethod perform :after
    ((o load-op)
     (c (eql (find-component "gandi-dns-updater" "public-stun-list.txt"))))
  (set (find-symbol* :*public-stun-servers* :gandi)
    (read-file-lines
     (component-pathname c))))
