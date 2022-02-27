(in-package :gandi)

(defvar *record-names* '("@"))

(defvar *log-format* '((:year 4) #\- (:month 2) #\- (:day 2)
                                 #\SPACE
                       (:hour 2) #\: (:min 2) #\: (:sec 2)))

;; should be loaded via asdf
(defvar *public-stun-servers*)
