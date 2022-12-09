(defun flatten (l)
  "Utility function for flattening a list of lists."
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun sign (x) (if (> x 0) 1 -1))

(defclass point ()
  ((x
    :initarg :x
    :initform 0
    :accessor x)
   (y
    :initarg :y
    :initform 0
    :accessor y)))

(defun new-point (x y)
  (make-instance 'point :x x :y y))

(defmethod subtract ((a point) (b point))
  (make-instance 'point
                 :x (- (x a) (x b))
                 :y (- (y a) (y b))))

(defmethod move ((p point) (dir point))
  (setf (x p) (+ (x p) (x dir)))
  (setf (y p) (+ (y p) (y dir))))

;; (defmethod diagonalp ((p point) (other point))
;;   (let ((delta (subtract p other)))
;;     (and (>= (abs (x delta)) 1) (>= (abs (y delta)) 1))))

(defun create-points (dir n)
  (loop for i from 0 to (1- n) collect (new-point (x dir) (y dir))))

(defun expand-command (line)
  (let ((dir (char line 0)) (n (digit-char-p (char line 2))))
    (cond
      ((string= dir "R") (create-points (new-point 1 0) n))
      ((string= dir "L") (create-points (new-point -1 0) n))
      ((string= dir "U") (create-points (new-point 0 -1) n))
      ((string= dir "D") (create-points (new-point 0 1) n))
      (t (error "invalid input when parsing command")))))

(defparameter *commands*
  (with-open-file (in "../data/day09.txt")
    (flatten
     (loop for line = (read-line in nil)
           while line
           when (> (length line) 0)
             collect (expand-command line)))))

(defun find-next-tail-move (rhead rtail)
  (let ((delta (subtract rhead rtail)))
    (cond
      ((and (and (>= (abs (x delta)) 1) (>= (abs (y delta)) 1))
            (or (= (abs (x delta)) 2) (= (abs (y delta)) 2)))
       (new-point (sign (x delta)) (sign (y delta))))
      ((>= (abs (x delta)) 2) (new-point (sign (x delta)) 0))
      ((>= (abs (y delta)) 2) (new-point 0 (sign (y delta))))
      (t (new-point 0 0)))))

(defun p1-find-visited-tail-positions ()
  (let ((positions '()) (head (new-point 0 0)) (tail (new-point 0 0)))
    (loop for command in *commands* do
      (move head command)
      (format t "H: ~a,~a~%" (x head) (y head))
      (let ((tail-move (find-next-tail-move head tail)))
        (move tail tail-move)
        (push (format nil "~a,~a" (x tail) (y tail)) positions))
      (format t "T: ~a,~a~%" (x tail) (y tail)))
    (length (delete-duplicates positions :test #'string=))))

