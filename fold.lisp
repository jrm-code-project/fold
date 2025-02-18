;;; -*- Lisp -*-

(in-package "FOLD")

(defparameter *truncate-fold* nil
  "If true, fold-left and fold-right on multiple sequences will
 stop folding when any of the sequences is exhausted.")

(declaim (ftype (function ((function (t t &rest t) t) t t &rest t) t) fold-left fold-right))

(defun fold-left (function initial list &rest lists)
  (labels ((fold-left-1 (state item tail)
             (declare (optimize (debug 0) (safety 0) (speed 3)))
             (cond ((consp tail)
                    (fold-left-1 (funcall function state item) (car tail) (cdr tail)))
                   ((null tail) (funcall function state item))
                   (t (cerror "Ignore dotted tail." "Dotted list encountered by fold-left.")
                      (funcall function state item))))

           (fold-left-1-simple-string (state string)
             (declare (optimize (debug 0) (safety 0) (speed 3))
                      (type simple-string string))
             (do ((state state (funcall function state (schar string index)))
                  (index 0 (1+ index)))
                 ((>= index (length string)) state)))

           (fold-left-1-simple-vector (state vector)
             (declare (optimize (debug 0) (safety 0) (speed 3))
                      (type simple-vector vector))
             (do ((state state (funcall function state (svref vector index)))
                  (index 0 (1+ index)))
                 ((>= index (length vector)) state)))

           (fold-left-1-sequence (state sequence)
             (do ((state state (funcall function state (elt sequence index)))
                  (index 0 (1+ index)))
                 ((>= index (length sequence)) state)))

           (fold-left-n (state items tails)
             (cond ((every #'consp tails)
                    (fold-left-n (apply function state items) (map 'list #'car tails) (map 'list #'cdr tails)))
                   ((or (every #'null tails)
                        *truncate-fold*)
                    (apply function state items))
                   (t (cerror "Truncate lists to same length."
                              "Lists of different lengths or dotted list in fold-left.")
                      (apply function state items))))

           (fold-left-n-sequence (state sequences)
             (cond (*truncate-fold*
                    (fold-left-n-sequence-loop state sequences 0 (apply #'min (map 'list #'length sequences))))
                   ((every (lambda (s) (= (length s) (length (car sequences)))) (cdr sequences))
                    (fold-left-n-sequence-loop state sequences 0 (length (car sequences))))
                   (t (cerror "Truncate sequences to shortest length."
                              "Sequences of different lengths in fold-left.")
                      (fold-left-n-sequence-loop state sequences 0 (apply #'min (map 'list #'length sequences))))))
           
           (fold-left-n-sequence-loop (state sequences index limit)
             (if (>= index limit)
                 state
                 (fold-left-n-sequence-loop (apply function state (map 'list (lambda (s) (elt s index)) sequences))
                                          sequences
                                          (1+ index)
                                          limit))))

    (if (null lists)
        (cond ((consp list) (fold-left-1 initial (car list) (cdr list)))
              ((typep list 'simple-string) (fold-left-1-simple-string initial list))
              ((typep list 'simple-vector) (fold-left-1-simple-vector initial list))
              ((typep list 'sequence) (fold-left-1-sequence initial list))
              ((null list) initial)
              (t (error "Non sequence in fold-left.")))
        (let ((tails (cons list lists)))
          (cond ((every #'consp tails)
                 (fold-left-n initial (map 'list #'car tails) (map 'list #'cdr tails)))
                ((every (lambda (tail) (typep tail 'sequence)) tails)
                 (fold-left-n-sequence initial tails))
                ((every #'null tails) initial)
                (t (error "Non sequence in fold-left.")))))))

(defun fold-right (function list final &rest args)
  (labels ((fold-right-1 (tail final)
             (cond ((consp tail)
                    (funcall function (car tail) (fold-right-1 (cdr tail) final)))
                   ((null tail) final)
                   (t (cerror "Ignore dotted tail."
                              "Dotted list encountered by fold-right.")
                      final)))

           (fold-right-1-sequence (sequence final)
             (do ((index (1- (length sequence)) (1- index))
                  (state final (funcall function (elt sequence index) state)))
                 ((< index 0) state)))

           (fold-right-n (tails final)
             (cond ((every #'consp tails)
                    (apply function (append (map 'list #'car tails)
                                            (list (fold-right-n (map 'list #'cdr tails) final)))))
                   ((or (every #'null tails)
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
        (cond ((consp list) (fold-right-1 list final))
              ((typep list 'sequence) (fold-right-1-sequence list final))
              ((null list) final)
              (t (error "Non sequence in fold-right.")))
        (let ((sequences (list* list final (butlast args)))
              (final* (car (last args))))
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
