(in-package :gandi)

(defvar *paths*
  (nconc (cons
	  (getcwd)
	  (mapcar
	   (compose #'(lambda (pn)
			(merge-pathnames
			 (ensure-directory-pathname #P"dns-updater")
			 pn))
		    #'ensure-directory-pathname)
	   (append
	    #+unix (list
		    #P"/etc/"
		    (or (getenv "XDG_CONFIG_HOME")
			(concatenate 'string
				     (getenv "HOME")
				     "/.config"))))))))

(defun find-config-file ()
  (let ((paths
	  (mapcar (compose
		   #'(lambda (pn)
		       (merge-pathnames pn #P"settings.toml")))
		  *paths*)))
    (find-if #'probe-file paths)))

(defun load-config ()
  (let* ((config-file (find-config-file))
	 (toml-string (read-file-into-string config-file))
	 (parsed (pp-toml:parse-toml toml-string)))
    (loop with config
	  for key being each hash-key in parsed using (hash-value value)
	  if (string= "gandi" key)
	    do (setf (getf config :gandi) value)
	  if (starts-with-subseq "domain" key)
	    do (push value
		     (getf config :domains))
	  finally (return config))))

(defvar *config*
    (load-config))

(defun record-url (record)
  (concatenate 'string
               (gandi-api-url)
	       "/"
               (first-domain-name)
               "/records/"
               record
               "/A"))

(defun record-urls ()
  (mapcar #'record-url *record-names*))

(defun format-with-timestamp (s &rest args)
  (format-timestring s (now) :format *log-format*)
  (format s " - ")
  (apply #'format (cons s args)))

(defun get-gandi-dns-ip ()
  (cadr (assoc :rrset--values
               (decode-json
                (http-request
		 (record-url "@")
                 :additional-headers `(("X-Api-Key" . ,(gandi-api-key)))
                 :want-stream t)))))

(defun update-gandi-dns-ips (new-ip)
  (mapc #'(lambda (url)
            (http-request
	     url
             :method :put
             :content-type "application/json"
             :additional-headers `(("X-Api-Key" . ,(gandi-api-key)))
             :content (lambda (s)
			(encode-json `((rrset_ttl . ,(first-domain-ttl))
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

(defun gandi-api-key (&optional (config *config*))
  (gethash "api-key" (getf config :gandi)))

(defun gandi-api-url (&optional (config *config*))
  (gethash "api-url" (getf config :gandi)))

(defun first-domain-name (&optional (config *config*))
  (gethash "name" (car (getf config :domains))))

(defun first-domain-ttl (&optional (config *config*))
  (gethash "ttl" (car (getf config :domains))))
