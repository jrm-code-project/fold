;;; -*- Lisp -*-

(defsystem "fold"
  :author "Joe Marshall <eval.apply@gmail.com>"
  :bug-tracker "https://github.com/jrm-code-project/fold/issues"
  :description "FOLD-LEFT and FOLD-RIGHT"
  :homepage "https://github.com/jrm-code-project/fold/"
  :license "MIT"
  :long-description "FOLD-LEFT and FOLD-RIGHT are higher-order functions that apply a function to a list of arguments in a left-to-right or right-to-left order."
  :mailto "eval.apply@gmail.com"
  :maintainer "Joe Marshall <eval.apply@gmail.com>"
  :source-control (:git "https://github.com/jrm-code-project/fold.git")
  :version "1.0.0"
  :components ((:file "fold" :depends-on ("package"))
               (:file "package")))
