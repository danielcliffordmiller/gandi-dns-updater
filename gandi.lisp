(defpackage :gandi
  (:use
   :cl-json
   :drakma
   :cl
   :local-time)
  (:export :run-check-and-update))

(in-package :gandi)

(defvar *api-key* "***_API_KEY_GOES_HERE_***")
(defvar *domain-name* "example.com")

(defvar *gandi-url* "https://dns.api.gandi.net/api/v5/domains/")

(defvar *domain-ttl* 1200)

(defvar *record-names* '("@"))

(defvar *log-format* '((:year 4) #\- (:month 2) #\- (:day 2)
                                 #\SPACE
                       (:hour 2) #\: (:min 2) #\: (:sec 2)))

(defun record-url (record)
  (concatenate 'string
               *gandi-url*
               *domain-name*
               "/records/"
               record
               "/A"))

(defun record-urls ()
  (mapcar #'record-url *record-names*))

(defun format-with-timestamp (s &rest args)
  (format-timestring s (now) :format *log-format*)
  (format s " - ")
  (apply #'format (cons s args)))

(defun get-ext-ip ()
  (http-request "http://bot.whatismyipaddress.com"))

(defun get-gandi-dns-ip ()
  (cadr (assoc :rrset--values
               (decode-json
                 (http-request (record-url "@")
                               :additional-headers `(("X-Api-Key" . ,*api-key*))
                               :want-stream t)))))

(defun update-gandi-dns-ips (new-ip)
  (mapc #'(lambda (url)
            (http-request url
                          :method :put
                          :content-type "application/json"
                          :additional-headers `(("X-Api-Key" . ,*api-key*))
                          :content (lambda (s) (encode-json `((rrset_ttl . ,*domain-ttl*)
                                                              (rrset_values . (,new-ip)))
                                                            s))))
        (record-urls)))

(defun run-check-and-update ()
  (let ((my-ip (get-ext-ip))
        (gandi-ip (get-gandi-dns-ip)))
    (format-with-timestamp t "ext ip: ~a, dns ip: ~a~%" my-ip gandi-ip)
    (if (not (equal my-ip gandi-ip))
        (progn
          (format-with-timestamp t "updating gandi dns~%")
          (update-gandi-dns-ips my-ip)))))
