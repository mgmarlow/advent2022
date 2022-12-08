(load "~/.quicklisp/setup.lisp")
(ql:quickload "cl-utilities")

(defclass command ()
  ((name
    :initarg :command
    :accessor command)
   (value
    :initarg :value
    :accessor value)))

(defclass resource ()
  ((kind
    :initarg :kind
    :accessor kind)
   (name
    :initarg name
    :accessor name)
   (size
    :initarg :size
    :initform 0
    :accessor size)
   (children
    :initarg :children
    :accessor children)))

(defun commandp (line)
  (string= (char line 0) #\$))

(defun new-command (line)
  (let ((parsed (cl-utilities:split-sequence #\SPACE line)))
    (if (string= (second parsed) "cd")
        (make-instance 'command
                       :name "cd"
                       :value (third parsed))
        (make-instance 'command
                       :name "ls"
                       :value '()))))

(defun new-resource (line)
  (let ((parsed (cl-utilities:split-sequence #\SPACE line)))
    (if (string= (first parsed) "dir")
        (make-instance 'resource
                       :kind directory
                       :name (second parsed))
        (make-instance 'resource
                       :kind file
                       :name (second parsed)
                       :size (first parsed)))))

;; (defparameter *raw-input*
;;   (with-open-file (in "../data/day06.txt")
;;     (loop for line = (read-line in nil)
;;           while line
;;           collect line)))

(defun build-file-system ()
  (let ((stack '()))
    (with-open-file (in "../data/day06.txt")
      (loop for line = (read-line in nil)
            while line
            when (> (length line) 0) do
              (cond
                ((commandp line) 'todo-create-command-and-update-state)
                (t 'todo-build-children))))
    'todo))

