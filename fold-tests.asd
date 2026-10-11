;;; -*- Lisp -*-

(defsystem "fold-tests"
  :description "Test suite for FOLD."
  :depends-on ("fold")
  :components ((:file "tests"))
  :perform (test-op (op c)
             (uiop:symbol-call "FOLD-TESTS" "RUN-TESTS")))
