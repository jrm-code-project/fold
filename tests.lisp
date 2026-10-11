;;; -*- Lisp -*-

(defpackage "FOLD-TESTS"
  (:use "COMMON-LISP" "FOLD")
  (:export "RUN-TESTS"))

(in-package "FOLD-TESTS")

(defvar *tests* '()
  "Registered test names, in definition order.")

(defmacro deftest (name &body body)
  `(progn
     (defun ,name () ,@body)
     (pushnew ',name *tests*)
     ',name))

(defmacro check (form)
  `(unless ,form
     (error "Check failed: ~S" ',form)))

(defmacro signals-error? (&body body)
  `(handler-case (progn ,@body nil)
     (error () t)))

(defun run-tests ()
  "Run every test; return T if all pass, otherwise signal an error after reporting."
  (let ((failures '()))
    (dolist (test (reverse *tests*))
      (handler-case (progn (funcall test)
                           (format t "PASS ~A~%" test))
        (error (e)
          (push test failures)
          (format t "FAIL ~A: ~A~%" test e))))
    (format t "~D tests, ~D failures~%" (length *tests*) (length failures))
    (when failures
      (error "Failed tests: ~S" (reverse failures)))
    t))

;;; fold-left

(deftest fold-left-list
  (check (eql (fold-left #'+ 0 '(1 2 3 4 5)) 15))
  (check (equal (fold-left (lambda (l r) `(f ,l ,r)) 'i '(a b c))
                '(f (f (f i a) b) c))))

(deftest fold-left-empty
  (check (eql (fold-left #'+ 7 '()) 7))
  (check (eql (fold-left #'+ 7 "") 7))
  (check (eql (fold-left #'+ 7 #()) 7)))

(deftest fold-left-string
  (check (equal (fold-left #'list 0 "12345")
                '(((((0 #\1) #\2) #\3) #\4) #\5))))

(deftest fold-left-vector
  (check (equal (fold-left #'list nil #(a b c)) '(((nil a) b) c))))

(deftest fold-left-non-simple-vector
  (check (equal (fold-left #'list nil
                           (make-array 3 :adjustable t :initial-contents '(a b c)))
                '(((nil a) b) c))))

(deftest fold-left-multiple-lists
  (check (equal (fold-left (lambda (acc one two) `(f ,acc ,one ,two))
                           'i '(a b c) '(do re mi))
                '(f (f (f i a do) b re) c mi))))

(deftest fold-left-mixed-sequences
  (check (equal (fold-left #'list nil '(a b c) #(d e f))
                '(((nil a d) b e) c f)))
  (check (equal (fold-left #'list nil '(a b c) "foo")
                '(((nil a #\f) b #\o) c #\o)))
  (check (equal (fold-left #'list nil "foo" "bar")
                '(((nil #\f #\b) #\o #\a) #\o #\r))))

(deftest fold-left-truncate
  (check (eql (let ((*truncate-fold* t))
                (fold-left #'+ 0 '(1 2 3 4 5) #(1 2 3 4) '(1 2 3)))
              18)))

(deftest fold-left-length-mismatch-errors
  (check (null (ignore-errors
                (let ((*truncate-fold* nil))
                  (fold-left #'+ 0 '(1 2 3 4 5) '(1 2 3 4) '(1 2 3 4)))))))

(deftest fold-left-dotted-list
  (check (equal (let ((*truncate-fold* t))
                  (fold-left #'list nil '(a b . c)))
                '((nil a) b)))
  (check (signals-error?
           (let ((*truncate-fold* nil))
             (fold-left #'list nil '(a b . c)))))
  (check (equal (let ((*truncate-fold* nil))
                  (handler-bind ((error #'continue))
                    (fold-left #'list nil '(a b . c))))
                '((nil a) b))))

(deftest fold-left-non-sequence
  (check (signals-error? (fold-left #'list nil 42))))

(deftest expand-fold-left-builds-forms
  (check (equal (fold::expand-fold-left 'f 'i '(a b c))
                '(f (f (f i a) b) c))))

;;; fold-right

(deftest fold-right-list
  (check (eql (fold-right #'+ '(1 2 3 4 5) 0) 15))
  (check (equal (fold-right (lambda (l r) `(f ,l ,r)) '(a b c) 'final)
                '(f a (f b (f c final))))))

(deftest fold-right-empty
  (check (eql (fold-right #'+ '() 7) 7))
  (check (eql (fold-right #'+ "" 7) 7))
  (check (eql (fold-right #'+ #() 7) 7)))

(deftest fold-right-string
  (check (equal (fold-right #'list "12345" 0)
                '(#\1 (#\2 (#\3 (#\4 (#\5 0))))))))

(deftest fold-right-vector
  (check (equal (fold-right #'list #(a b c) nil) '(a (b (c nil))))))

(deftest fold-right-multiple-sequences
  (check (equal (fold-right #'list '(a b c) #(d e f) nil)
                '(a d (b e (c f nil)))))
  (check (equal (fold-right #'list '(a b c) '(d e f) nil)
                '(a d (b e (c f nil))))))

(deftest fold-right-truncate
  (check (eql (let ((*truncate-fold* t))
                (fold-right #'+ '(1 2 3 4 5) #(1 2 3 4) '(1 2 3) 0))
              18)))

(deftest fold-right-length-mismatch-errors
  (check (null (ignore-errors
                (let ((*truncate-fold* nil))
                  (fold-right #'+ '(1 2 3 4 5) '(1 2 3 4) '(1 2 3 4) 0))))))

(deftest fold-right-non-sequence
  (check (signals-error? (fold-right #'list 42 nil))))

;;; alist, plist, hash-table

(deftest alist-folds
  (let ((alist '((a . 1) (b . 2) (c . 3))))
    (check (equal (alist-fold-left (lambda (acc k v) (cons (list k v) acc)) nil alist)
                  '((c 3) (b 2) (a 1))))
    (check (equal (alist-fold-right (lambda (k v acc) (cons (list k v) acc)) alist nil)
                  '((a 1) (b 2) (c 3))))
    (check (eql (alist-fold-left (lambda (acc k v) (declare (ignore k)) (+ acc v)) 0 alist)
                6))))

(deftest plist-folds
  (let ((plist '(a 1 b 2 c 3)))
    (check (equal (plist-fold-left (lambda (acc k v) (cons (list k v) acc)) nil plist)
                  '((c 3) (b 2) (a 1))))
    (check (equal (plist-fold-right (lambda (k v acc) (cons (list k v) acc)) plist nil)
                  '((a 1) (b 2) (c 3))))
    (check (eql (plist-fold-left (lambda (acc k v) (declare (ignore k)) (+ acc v)) 0 '())
                0))))

(deftest plist-improper
  (check (signals-error? (plist-fold-left (lambda (acc k v) (declare (ignore k v)) acc)
                                          nil '(a 1 b))))
  (check (signals-error? (plist-fold-left (lambda (acc k v) (declare (ignore k v)) acc)
                                          nil '(a 1 . b)))))

(deftest hash-table-folds
  (let ((table (make-hash-table)))
    (setf (gethash 'a table) 1
          (gethash 'b table) 2
          (gethash 'c table) 3)
    (check (eql (hash-table-fold-left (lambda (acc k v) (declare (ignore k)) (+ acc v))
                                      0 table)
                6))
    (check (eql (hash-table-fold-right (lambda (k v acc) (declare (ignore k)) (+ acc v))
                                       table 0)
                6))
    (check (equal (sort (hash-table-fold-left (lambda (acc k v) (declare (ignore v))
                                                (cons k acc))
                                              nil table)
                        #'string< :key #'symbol-name)
                  '(a b c)))
    (check (eql (hash-table-fold-left #'+ 5 (make-hash-table)) 5))))
