gandi-dns-updater
=================

This is a simple script to call gandi's DNS api and update my dns entry if necessary.

To build this app, you could run something like:
```
buildapp --load-system gandi-dns-updater \
         --asdf-tree ~/quicklisp/dists/quicklisp/software/ \
         --eval '(defun main (args) (gandi:run-check-and-update))' \
         --output gandi-dns-updater \
         --entry main
```
