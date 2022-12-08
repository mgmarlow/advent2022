(load "~/.quicklisp/setup.lisp")
(ql:quickload "cl-utilities")

(defclass directory-node ()
  ((name
    :initarg :name
    :accessor name)
   (children
    :initarg :children
    :initform '()
    :accessor children)
   (parent
    :initarg :parent
    :accessor parent)
   (size
    :initarg :size
    :initform 0
    :accessor size)))

(defclass file-leaf ()
  ((name
    :initarg :name
    :accessor name)
   (size
    :initarg :size
    :accessor size)))

(defclass command ()
  ((kind
    :initarg :kind
    :accessor kind)
   (value
    :initarg :value
    :accessor value)))

(defun cd-commandp (parsed-line)
  (string= (second parsed-line) "cd"))

(defun commandp (line)
  (string= (char line 0) #\$))

(defun parse-commands ()
  (let ((commands '()) (latest-ls))
    (with-open-file (in "../data/day07.txt")
      (loop for line = (read-line in nil)
            while line
            when (> (length line) 0) do
              (cond ((commandp line)
                     (let ((parsed (cl-utilities:split-sequence #\SPACE line)))
                       (if (cd-commandp parsed)
                           (push (make-instance 'command
                                                :kind "cd"
                                                :value (third parsed))
                                 commands)
                           (progn
                             (let ((ls-command (make-instance 'command :kind "ls" :value '()))) 
                               (push ls-command commands)
                               (setf latest-ls ls-command))))))
                    (t (push line (value latest-ls)))))
      (nreverse commands))))

(defun build-file-system ()
  (let* ((root (make-instance 'directory-node :name "/")) (cwd root))
    ;; todo
    root))

