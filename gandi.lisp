(defpackage :gandi
  (:use :cl-json :drakma :cl :local-time)
  (:export :run-check-and-update))

(in-package :gandi)

(defvar *api-key* "***_API_KEY_GOES_HERE_***")
(defvar *domain-name* "example.com")

(defvar *gandi-url*
  (cl:concatenate 'string
                  "https://dns.api.gandi.net/api/v5/domains/"
                  *domain-name*
                  "/records/%40/A"))

(defvar *log-format* '((:year 4) #\- (:month 2) #\- (:day 2)
                                 #\SPACE
                       (:hour 2) #\: (:min 2) #\: (:sec 2)))

(defun format-with-timestamp (s &rest args)
  (local-time:format-timestring s (now) :format *log-format*)
  (cl:format s " - ")
  (apply #'cl:format (cons s args)))

(defun get-ext-ip ()
  (drakma:http-request "http://bot.whatismyipaddress.com"))

(defun get-gandi-dns-ip ()
  (cadr (assoc :rrset--values
               (cl-json:decode-json
                 (drakma:http-request *gandi-url*
                                      :additional-headers `(("X-Api-Key" . ,*api-key*))
                                      :want-stream t)))))

(defun update-gandi-dns-ip (new-ip)
  (drakma:http-request *gandi-url*
                       :method :put
                       :content-type "application/json"
                       :additional-headers `(("X-Api-Key" . ,*api-key*))
                       :content (lambda (s) (cl-json:encode-json `((rrset_ttl . 10800)
                                                                   (rrset_values . (,new-ip)))
                                                                 s))))

(defun run-check-and-update ()
  (let ((my-ip (get-ext-ip))
        (gandi-ip (get-gandi-dns-ip)))
    (format-with-timestamp t "ext ip: ~a, dns ip: ~a~%" my-ip gandi-ip)
    (if (not (equal my-ip gandi-ip))
        (progn
          (format-with-timestamp t "updating gandi dns~%")
          (update-gandi-dns-ip my-ip)))))
