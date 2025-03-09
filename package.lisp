;;; -*- Lisp -*-

(defpackage "FOLD"
  (:documentation "Implementation of FOLD-LEFT and FOLD-RIGHT.")
  (:export "ALIST-FOLD-LEFT"
           "ALIST-FOLD-RIGHT"
           "FOLD-LEFT"
           "FOLD-RIGHT"
           "HASH-TABLE-FOLD-LEFT"
           "HASH-TABLE-FOLD-RIGHT"
           "PLIST-FOLD-LEFT"
           "PLIST-FOLD-RIGHT"
           "*TRUNCATE-FOLD*")
  (:use "COMMON-LISP"))
