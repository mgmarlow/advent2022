(ql:quickload "cl-utilities")

(defun lastcar (lst) (first (last lst)))

(defun parse-line-points (line)
  (mapcar #'(lambda (str)
              (mapcar #'parse-integer (cl-utilities:split-sequence #\, str)))
          (remove-if #'(lambda (str) (equal str "->"))
                     (cl-utilities:split-sequence #\SPACE line))))

(defparameter *line-points*
  (with-open-file (in "../data/day14.txt")
    (loop for line = (read-line in nil)
          while line
          when (> (length line) 0)
            collect (parse-line-points line))))
