(ql:quickload "cl-utilities")

(defun lastcar (lst) (first (last lst)))

(defun parse-line (line)
  (let ((parsed (cl-utilities:split-sequence #\: line)))
    (cons (first parsed) (subseq (lastcar parsed) 1))))

(defparameter *input*
  (with-open-file (in "../data/day21.txt")
    (loop for line = (read-line in nil)
          while line
          collect (parse-line line))))

(defparameter *commands-hash*
  (let ((hsh (make-hash-table :test 'equal)))
    (loop for pair in *input* do
      (let ((key (car pair)) (value (cdr pair)))
        (setf (gethash key hsh) value)))
    hsh))

(defun evaluate (expression)
  (handler-case (parse-integer expression)
    (parse-error ()
      (let ((split-expression (cl-utilities:split-sequence #\Space expression)))
        (cond
          ((and (equal expression "humn") (not (null *human-input*))) *human-input*)
          ((= (length split-expression) 1)
           (evaluate (gethash expression *commands-hash*)))
          ((equal "+" (second split-expression))
           (+ (evaluate (first split-expression)) (evaluate (lastcar split-expression))))
          ((equal "-" (second split-expression))
           (- (evaluate (first split-expression)) (evaluate (lastcar split-expression))))
          ((equal "*" (second split-expression))
           (* (evaluate (first split-expression)) (evaluate (lastcar split-expression))))
          ((equal "/" (second split-expression))
           (/ (evaluate (first split-expression)) (evaluate (lastcar split-expression)))))))))

;; P2: TODO: evaluate equation for humn
(defun find-humn-shout ())

(format t "P1: ~a; P2: ~a~%" (evaluate "root") (find-humn-shout))
