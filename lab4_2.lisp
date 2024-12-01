(defun add-next-reducer (&key (transform #'identity))
  "For correct work you must use `reducer` with parameters:
    -:from-end t
    -:initial-value nil"
  (lambda (elem acc)
    (let ((next (if acc (car acc) '(nil))))
      (cons (cons (funcall transform elem) (car next)) acc))))

(defun check-reducer (name input-lst expected &key (transform #'identity)) 
  "Execute `reducer' on `input', compare result with `expected' and print comparison status" 
  (let ((result (reduce (add-next-reducer :transform transform)
                        input-lst
                        :initial-value nil
                        :from-end t)))
    (format t "~:[FAILED~;passed~]... ~a~%" 
                  (equal result expected) 
                  name)))

(defun test-reducer ()
  (check-reducer "test-1"
                 '(1 2 3)
                 '((1 . 2) (2 . 3) (3 . nil)))
  (check-reducer "test-2" 
                 '(1 2 3) 
                 '((10 . 20) (20 . 30) (30 . nil))
                 :transform (lambda (x) (* x 10)))
  (check-reducer "test-3"
                 '(42)
                 '((42 . nil)))
  (check-reducer "test-4"
                 '(-1 -2 -3)
                 '((1 . 2) (2 . 3) (3 . nil))
                 :transform #'abs)
  (check-reducer "test-5"
                 '()
                 '()))
