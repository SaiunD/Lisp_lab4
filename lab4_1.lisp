(defun shake-it (lst R i k &key (key #'identity) (test #'<))
  (if (and lst (cdr lst))
      (if (< i R)
          (let* ((z1 (car lst))
                 (z2 (cadr lst))
                 (key-z1 (funcall key z1))
                 (key-z2 (funcall key z2))
                 (cond1 (funcall test key-z2 key-z1))
                 (z (if cond1 z2 z1))
                 (key-z (if cond1 key-z2 key-z1))
                 (new-lst (if cond1 (cons z1 (cddr lst)) (cdr lst)))
                 (new-k (if cond1 i k)))
            (multiple-value-bind (temp-lst bind-k bind-R)
                (shake-it new-lst R (1+ i) new-k :key key :test test)
              (if (> i bind-R)
                  (values (cons z temp-lst) bind-k bind-R)
                  (let* ((key-temp (if temp-lst (funcall key (car temp-lst))))
                         (cond2 (and temp-lst (funcall test key-z key-temp)))
                         (back-k (if cond2 bind-k i)))
                    (if cond2
                        (values (cons z temp-lst) back-k bind-R)
                        (values (cons (car temp-lst) (cons z (cdr temp-lst))) back-k bind-R))))))
          (values lst k k))
      (values lst k k)))

(defun left-side-apart (lst L)
  (values (subseq lst 0 L) (nthcdr L lst)))

(defun shaker-sort-inner (lst L R k &key (key #'identity) (test #'<))
  (if (< L R)
      (multiple-value-bind (left-side right-side) (left-side-apart lst L)
        (multiple-value-bind (res res-k res-R) (shake-it right-side R L k :key key :test test)
          (shaker-sort-inner (append left-side res) (1+ res-k) res-R res-k :key key :test test)))
      lst))

(defun shaker-sort (lst &key (key #'identity) (test #'<))
  (shaker-sort-inner lst 0 (1- (length lst)) 0 :key key :test test))

(defun check-shaker-sort (name input-lst expected &key (key #'identity) (test #'<)) 
  "Execute `shaker-sort' on `input', compare result with `expected' and print comparison status" 
  (format t "~:[FAILED~;passed~]... ~a~%" 
          (equal (shaker-sort input-lst :key key :test test) expected) 
          name))

(defun test-shaker-sort ()
  (check-shaker-sort "test-1"
                     '(2 5 3 1 4 7 3 5 6 2)
                     '(1 2 2 3 3 4 5 5 6 7))
  (check-shaker-sort "test-2"
                     '(3 -5 6 -7 1 -2 2 4 8)
                     '(1 -2 2 3 4 -5 6 -7 8)
                     :key #'abs)
  (check-shaker-sort "test-3"
                     '(3 -5 6 -7 1 -2 2 4 8)
                     '(8 -7 6 -5 4 3 -2 2 1)
                     :key #'abs
                     :test #'>)
  (check-shaker-sort "test-4"
                     '((2 . 1) (5 . 2) (1 . 3) (6 . 4) (4 . 5) (3 . 6))
                     '((1 . 3) (2 . 1) (3 . 6) (4 . 5) (5 . 2) (6 . 4))
                     :key #'car)
  (check-shaker-sort "test-4"
                     '((2 . 1) (5 . 2) (1 . 3) (6 . 4) (4 . 5) (3 . 6))
                     '((3 . 6) (4 . 5) (6 . 4) (1 . 3) (5 . 2) (2 . 1))
                     :key #'cdr
                     :test #'>)
  (check-shaker-sort "test-5"
                     '()
                     '()))
