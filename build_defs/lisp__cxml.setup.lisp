(eval-when (:compile-toplevel :execute)
  (defpackage :cxml-setup
    (:use :common-lisp :uiop :asdf)
    (:export #:setup))

  (in-package :cxml-setup))

(eval-when (:compile-toplevel :execute)
  (defun setup-helper (catalog-pathname destination-pathname)
    (uiop:copy-file
     catalog-pathname
     (subpathname
      (subpathname (getcwd) (ensure-directory-pathname destination-pathname))
      (file-namestring catalog-pathname))))

  (defmacro setup (catalog dest)
    `(eval-when (:compile-toplevel :execute)
       (setup-helper ,catalog ,dest))))
