
gandi-dns-updater: gandi.lisp
	buildapp --load-system gandi-dns-updater \
		 --asdf-tree ~/quicklisp/dists/quicklisp/software/ \
		 --eval '(defun main (args) (gandi:run-check-and-update))' \
		 --output gandi-dns-updater \
		 --entry main
