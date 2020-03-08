
gandi-dns-updater: gandi.lisp
	cl-launch --quicklisp \
	   	  --system-package gandi-dns-updater \
	   	  --package gandi \
	   	  --output gandi-dns-updater \
	   	  --restart run-check-and-update
