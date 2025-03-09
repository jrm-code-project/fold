;;; -*- Lisp -*-

(in-package "FOLD")

(defparameter *truncate-fold* t
  "If true, fold-left and fold-right on multiple sequences will
 stop folding when any of the sequences is exhausted.")

(declaim (ftype (function ((function (t t &rest t) t) t t &rest t) t) fold-left fold-right))

(defun fold-left (function initial list &rest lists)
  (labels ((fold-left-1 (tail)
             (declare (optimize (debug 0) (safety 0) (speed 3)))
             (do ((state initial (funcall function state item))
                  (item (car tail) (car tail))
                  (tail (cdr tail) (cdr tail)))
                 ((not (consp tail))
                  (unless (or (null tail)
                              *truncate-fold*)
                    (cerror "Ignore dotted tail."
                            "Dotted list in fold-left."))
                  (funcall function state item))))

           (fold-left-1-simple-string (string)
             (declare (optimize (debug 0) (safety 0) (speed 3))
                      (type simple-string string))
             (do ((state initial (funcall function state (schar string index)))
                  (index 0 (1+ index)))
                 ((>= index (length string)) state)))

           (fold-left-1-simple-vector (vector)
             (declare (optimize (debug 0) (safety 0) (speed 3))
                      (type simple-vector vector))
             (do ((state initial (funcall function state (svref vector index)))
                  (index 0 (1+ index)))
                 ((>= index (length vector)) state)))

           (fold-left-1-sequence (sequence)
             (do ((state initial (funcall function state (elt sequence index)))
                  (index 0 (1+ index)))
                 ((>= index (length sequence)) state)))

           (fold-left-n (lists)
             (do ((state initial (apply function state items))
                  (items (map 'list #'car lists) (map 'list #'car tails))
                  (tails (map 'list #'cdr lists) (map 'list #'cdr tails)))
                 ((not (every #'consp tails))
                  (unless (or (every #'null tails)
                              *truncate-fold*)
                    (cerror "Truncate lists to same length."
                            "Lists of different lengths or dotted list in fold-left."))
                  (apply function state items))))

           (fold-left-n-sequence (sequences)
             (cond (*truncate-fold*
                    (fold-left-n-sequence-loop sequences (apply #'min (map 'list #'length sequences))))
                   ((every (lambda (s) (= (length s) (length (car sequences)))) (cdr sequences))
                    (fold-left-n-sequence-loop sequences (length (car sequences))))
                   (t (cerror "Truncate sequences to shortest length."
                              "Sequences of different lengths in fold-left.")
                      (fold-left-n-sequence-loop sequences (apply #'min (map 'list #'length sequences))))))
           
           (fold-left-n-sequence-loop (sequences limit)
             (do ((index 0 (1+ index))
                  (state initial (apply function state (map 'list (lambda (s) (elt s index)) sequences))))
                 ((>= index limit) state))))

    (if (null lists)
        (cond ((consp list)                (fold-left-1 list))
              ((typep list 'simple-string) (fold-left-1-simple-string list))
              ((typep list 'simple-vector) (fold-left-1-simple-vector list))
              ((typep list 'sequence)      (fold-left-1-sequence list))
              ((null list)                 initial)
              (t (error "Non sequence in fold-left.")))
        (let ((lists* (cons list lists)))
          (cond ((every #'consp lists*)
                 (fold-left-n lists*))
                ((every (lambda (list) (typep list 'sequence)) lists*)
                 (fold-left-n-sequence lists*))
                ((every #'null lists*) initial)
                (t (error "Non sequence in fold-left.")))))))

#||
(defun test-fold-left ()
  (assert (eql (fold-left #'+ 0 '(1 2 3 4 5)) 15))
  (assert (equal (fold-left #'list 0 "12345") '(((((0 #\1) #\2) #\3) #\4) #\5)))
  (assert (equal (fold-left #'list nil #(a b c)) '(((NIL A) B) C)))
  (assert (equal (fold-left #'list nil '(a b c) #(d e f)) '(((NIL A D) B E) C F)))
  (assert (equal (let ((*truncate-fold* t))
                   (fold-left #'+ 0 '(1 2 3 4 5) #(1 2 3 4) '(1 2 3)))
                 18))
  (assert (equal (ignore-errors
                  (let ((*truncate-fold* nil))
                    (fold-left #'+ 0 '(1 2 3 4 5) '(1 2 3 4) '(1 2 3 4))))
                 nil)))
||#

(defun fold-right (function list final &rest args)
  (labels ((fold-right-1 (list)
             (cond ((consp list)
                    (funcall function (car list) (fold-right-1 (cdr list))))
                   ((null list) final)
                   (t (cerror "Ignore dotted tail."
                              "Dotted list encountered by fold-right.")
                      final)))

           (fold-right-1-sequence (sequence)
             (do ((index (1- (length sequence)) (1- index))
                  (state final (funcall function (elt sequence index) state)))
                 ((< index 0) state)))

           (fold-right-n (lists final)
             (cond ((every #'consp lists)
                    (apply function (append (map 'list #'car lists)
                                            (list (fold-right-n (map 'list #'cdr lists) final)))))
                   ((or (every #'null lists)
                        *truncate-fold*)
                    final)
                   (t (cerror "Truncate longer lists."
                              "Lists of different lengths or dotted list in fold-right.")
                      final)))

           (fold-right-n-sequences (sequences final start)
             (do ((index (1- start) (1- index))
                  (state final (apply function (append (map 'list (lambda (s) (elt s index)) sequences)
                                                       (list state)))))
                 ((< index 0) state))))

          (if (null args)
              (cond ((consp list)           (fold-right-1 list))
                    ((null list) final)
                    ((typep list 'sequence) (fold-right-1-sequence list))
                    (t (error "Non sequence in fold-right.")))
              (let ((sequences (list* list final (butlast args)))
                    (final*    (car (last args))))
                (cond ((every #'consp sequences) (fold-right-n sequences final*))
                      ((every (lambda (s) (typep s 'sequence)) sequences)
                       (cond (*truncate-fold*
                              (fold-right-n-sequences sequences final*
                                                      (apply #'min (map 'list #'length sequences))))
                             ((every (lambda (s) (= (length s) (length (car sequences)))) sequences)
                              (fold-right-n-sequences sequences final*
                                                      (length (car sequences))))
                             (t (cerror "Truncate longer sequences."
                                        "Sequences of different lengths in fold-right.")
                                (fold-right-n-sequences sequences final*
                                                        (apply #'min (map 'list #'length sequences))))))
                      ((every #'null sequences) final*)
                      (t (error "Non sequence in fold-right.")))))))

#||
(defun test-fold-right ()
  (assert (eql (fold-right #'+ '(1 2 3 4 5) 0) 15))
  (assert (equal (fold-right #'list "12345" 0) '(#\1 (#\2 (#\3 (#\4 (#\5 0)))))))
  (assert (equal (fold-right #'list #(a b c) nil) '(A (B (C NIL)))))
  (assert (equal (fold-right #'list '(a b c) #(d e f) nil) '(A D (B E (C F NIL)))))
  (assert (equal (let ((*truncate-fold* t))
                   (fold-right #'+ '(1 2 3 4 5) #(1 2 3 4) '(1 2 3) 0))
                 18))
  (assert (equal (ignore-errors
                  (let ((*truncate-fold* nil))
                    (fold-right #'+ '(1 2 3 4 5) '(1 2 3 4) '(1 2 3 4) 0)))
                 nil)))
||#

(defun alist-fold-left (fn init alist)
  (fold-left (lambda (acc entry)
               (funcall fn acc (car entry) (cdr entry)))
             init
             alist))

(defun alist-fold-right (fn alist final)
  (fold-left (lambda (acc entry)
                (funcall fn (car entry) (cdr entry) acc))
              final
              (reverse alist)))

(defun hash-table-fold-left (fn init hashtable)
  (check-type fn function)
  (with-hash-table-iterator (next hashtable)
    (labels ((iter (fn acc)
               (declare (type function fn))
               (multiple-value-bind (entry? key value) (next)
                 (if entry?
                     (iter fn (funcall fn acc key value))
                     acc))))
      (iter fn init))))

(defun hash-table-fold-right (fn hashtable final)
  (check-type fn function)
  (with-hash-table-iterator (next hashtable)
    (labels ((iter (fn acc)
               (declare (type function fn))
               (multiple-value-bind (entry? key value) (next)
                 (if entry?
                     (iter fn (funcall fn key value acc))
                     acc))))
      (iter fn final))))

(defun plist-fold-left (fn init plist)
  (cond ((consp plist) (if (consp (cdr plist))
                           (plist-fold-left fn (funcall fn init (car plist) (cadr plist))
                                            (cddr plist))
                           (error "Improper plist.")))
        ((null plist) init)
        (t (error "Improper plist."))))

(defun plist-fold-right (fn plist final)
  (plist-fold-left (lambda (acc value key)
                     (funcall fn key value acc))
                   final
                   (reverse plist)))
