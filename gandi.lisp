(defpackage :gandi
  (:use :cl-json :drakma :cl :local-time)
  (:export :run-check-and-update))

(in-package :gandi)

(defvar *api-key* "***_API_KEY_GOES_HERE_***")
(defvar *domain-name* "example.com")

(defvar *gandi-url* "https://dns.api.gandi.net/api/v5/domains/")

;(defvar *gandi-url*
;  (cl:concatenate 'string
;                  "https://dns.api.gandi.net/api/v5/domains/"
;                  *domain-name*
;                  "/records/%40/A"))

(defvar *record-names* '("@" "files"))

(defvar *log-format* '((:year 4) #\- (:month 2) #\- (:day 2)
                                 #\SPACE
                       (:hour 2) #\: (:min 2) #\: (:sec 2)))

(defun record-url (record)
  (cl:concatenate 'string
                  *gandi-url*
                  *domain-name*
                  "/records/"
                  record
                  "/A"))

(defun record-urls ()
  (mapcar #'record-url *record-names*))

(defun format-with-timestamp (s &rest args)
  (local-time:format-timestring s (now) :format *log-format*)
  (cl:format s " - ")
  (apply #'cl:format (cons s args)))

(defun get-ext-ip ()
  (drakma:http-request "http://bot.whatismyipaddress.com"))

(defun get-gandi-dns-ip ()
  (cadr (assoc :rrset--values
               (cl-json:decode-json
                 (drakma:http-request (record-url "@")
                                      :additional-headers `(("X-Api-Key" . ,*api-key*))
                                      :want-stream t)))))

(defun update-gandi-dns-ips (new-ip)
  (mapc #'(lambda (url)
            (drakma:http-request url
                                 :method :put
                                 :content-type "application/json"
                                 :additional-headers `(("X-Api-Key" . ,*api-key*))
                                 :content (lambda (s) (cl-json:encode-json `((rrset_ttl . 10800)
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
