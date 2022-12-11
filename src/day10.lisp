(ql:quickload "cl-utilities")

(defun units-of-work (command)
  (cond
    ((string= command "noop") 1)
    ((string= command "addx") 2)
    (t 0)))

(defclass operation ()
  ((work
    :initarg :work
    :accessor work)
   (command
    :initarg :command
    :accessor command)
   (value
    :initarg :value
    :accessor value)))

(defun new-operation (line)
  (let* ((parsed (cl-utilities:split-sequence #\SPACE line))
         (command (first parsed))
         (value (second parsed)))
    (make-instance 'operation
                   :work (units-of-work command)
                   :command command
                   :value (when value (parse-integer value)))))

(defun read-instructions ()
  (with-open-file (in "../data/day10.txt")
    (loop for line = (read-line in nil)
          while line
          when (> (length line) 0)
            collect (new-operation line))))

(defclass cpu ()
  ((counter
    :initform 0
    :accessor counter)
   (cycle
    :initform 1
    :accessor cycle)
   (regx
    :initform 1
    :accessor regx)
   (current-instruction
    :accessor current-instruction)))

(defmethod process-instruction ((m cpu) instruction)
  (cond
    ;; ignore noop
    ((string= "addx" (command instruction)) (incf (regx m) (value instruction)))))

;; look at clock & current-instruction here
(defmethod run ((m cpu) instructions cb)
  (let ((current-instruction))
    (loop while (> (length instructions) 0) do
      (funcall cb m)
      (unless current-instruction
        (setf current-instruction (pop instructions))
        (setf (counter m) (work current-instruction)))
      (decf (counter m))
      (when (= 0 (counter m))
        (process-instruction m current-instruction)
        (setf current-instruction nil))
      (incf (cycle m)))))

(defmethod signal-strength ((m cpu))
  (* (cycle m) (regx m)))

(defun p1-cycles ()
  (let ((machine (make-instance 'cpu)) (sum 0))
    (run machine (read-instructions) #'(lambda (m)
                                         (when (find (cycle m) '(20 60 100 140 180 220))
                                           (incf sum (signal-strength m)))))
    sum))

(format t "P1: ~a; P2: ~a~%" (p1-cycles) 'todo)
