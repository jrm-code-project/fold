;;; -*- Lisp -*-

(in-package "FOLD")

(defparameter *truncate-fold* nil
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
                                            (list (fold-right-n (map 'list #'cdr lists))))))
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
              ((typep list 'sequence) (fold-right-1-sequence list))
              ((null list) final)
              (t (error "Non sequence in fold-right.")))
        (let ((sequences (list* list final (butlast args)))
              (final*    (car (last args))))
          (cond ((every #'consp sequences) (fold-right-n sequences final*))
                ((every (lambda (s) (typep s 'sequence)) sequences)
                 (cond (*truncate-fold*
                        (fold-right-n-sequences sequences final* (apply #'min (map 'list #'length sequences))))
                       ((every (lambda (s) (= (length s) (length (car sequences)))) sequences)
                        (fold-right-n-sequences sequences final* (length (car sequences))))
                       (t (cerror "Truncate longer sequences."
                                  "Sequences of different lengths in fold-right.")
                          (fold-right-n-sequences sequences final* (apply #'min (map 'list #'length sequences))))))
                ((every #'null sequences) final*)
                (t (error "Non sequence in fold-right.")))))))
