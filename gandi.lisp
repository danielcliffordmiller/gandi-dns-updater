(in-package :gandi)

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

;;(defun get-ext-ip ()
;;  (http-request *ip-detect-service*))

(defun get-gandi-dns-ip ()
  (cadr (assoc :rrset--values
               (decode-json
                (http-request
		 (record-url "@")
                 :additional-headers `(("X-Api-Key" . ,*api-key*))
                 :want-stream t)))))

(defun update-gandi-dns-ips (new-ip)
  (mapc #'(lambda (url)
            (http-request
	     url
             :method :put
             :content-type "application/json"
             :additional-headers `(("X-Api-Key" . ,*api-key*))
             :content (lambda (s)
			(encode-json `((rrset_ttl . ,*domain-ttl*)
                                       (rrset_values . (,new-ip)))
                                     s))))
        (record-urls)))

(defun run-check-and-update ()
  (let ((my-ip (get-ext-ip))
        (gandi-ip (get-gandi-dns-ip)))
    (when my-ip
      (format-with-timestamp t "ext ip: ~a, dns ip: ~a~%" my-ip gandi-ip)
      (if (not (equal my-ip gandi-ip))
          (progn
            (format-with-timestamp t "updating gandi dns~%")
            (update-gandi-dns-ips my-ip))))))

(defun random-stun-server ()
  (let ((entry (split-sequence #\: (random-elt *public-stun-servers*))))
    (list (first entry)
	  (parse-integer (second entry)))))

(defun get-ext-ip ()
  "get public facing ip address from the stun server list"
  (let ((server (random-stun-server)))
    (format t "calling: ~a~%" server)
    (ignore-errors
     (let ((address (get-mapped-address
		     (apply #'bind-request server))))
       (when (= 4 (length (car address))) ; so far only support ip4
	(reduce #'(lambda (a b) (concatenate 'string a "." b))
		(map 'list #'princ-to-string (car address))))))))
