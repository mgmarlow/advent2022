(defun foreach-line (file fn)
  (with-open-file (in file)
    (when in
      (loop for line = (read-line in nil)
            while line do (funcall fn line)))))

(defun foreach-sum (file fn)
  (let ((current 0))
    (foreach-line file
                  #'(lambda (line)
                      (if (= (length line) 0)
                          (progn
                            (funcall fn current)
                            (setf current 0))
                          (incf current (parse-integer line)))))))

(defun totals ()
  (let ((totals ()))
    (foreach-sum "./data/day01.txt"
                 #'(lambda (sum) (setf totals (cons sum totals))))
    totals))

(defun day1part1 ()
  (reduce #'max (totals)))

(defun day1part2 ()
  (let ((sorted-totals (sort (totals) #'>)))
    (+ (first sorted-totals) (second sorted-totals) (third sorted-totals))))

(format t "P1: ~a; P2: ~a~%" (day1part1) (day1part2))
