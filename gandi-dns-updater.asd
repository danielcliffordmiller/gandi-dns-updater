(defsystem "gandi-dns-updater"
  :description "script to update gandi-dns"
  :version "0.0.2"
  :author "Dan Miller"
  :depends-on (:drakma :cl-json :local-time)
  :components ((:file "gandi")))

