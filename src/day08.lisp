(defparameter *matrix*
  (with-open-file (in "./data/day08.txt")
    (loop for line = (read-line in nil)
          while line
          when (> (length line) 0)
            collect (map 'list #'digit-char-p line))))

(defmacro matrix-get (x y matrix)
  `(nth ,x (nth ,y ,matrix)))

(defun foreach-up (x y fn)
  (loop for i from (1- y) downto 0 do
    (let ((other (matrix-get x i *matrix*)))
      (funcall fn other))))

(defun foreach-right (x y fn)
  (loop for i from (1+ x) to (1- (length (first *matrix*))) do
    (let ((other (matrix-get i y *matrix*)))
      (funcall fn other))))

(defun foreach-down (x y fn)
  (loop for i from (1+ y) to (1- (length *matrix*)) do
    (let ((other (matrix-get x i *matrix*)))
      (funcall fn other))))

(defun foreach-left (x y fn)
  (loop for i from (1- x) downto 0 do
    (let ((other (matrix-get i y *matrix*)))
      (funcall fn other))))

(defun direction-visiblep (dirfn x y item)
  (let ((any-taller))
    (funcall dirfn x y #'(lambda (other)
                           (when (>= other item)
                             (setf any-taller t))))
    (not any-taller)))

(defun top-visiblep (x y item)
  (direction-visiblep #'foreach-up x y item))

(defun right-visiblep (x y item)
  (direction-visiblep #'foreach-right x y item))

(defun bottom-visiblep (x y item)
  (direction-visiblep #'foreach-down x y item))

(defun left-visiblep (x y item)
  (direction-visiblep #'foreach-left x y item))

(defun visiblep (x y)
  (let ((item (matrix-get x y *matrix*)))
    (cond
      ((= x 0) t)
      ((= x (1- (length (first *matrix*)))) t)
      ((= y 0) t)
      ((= y (1- (length *matrix*))) t)
      (t  (or (top-visiblep x y item)
              (right-visiblep x y item)
              (bottom-visiblep x y item)
              (left-visiblep x y item))))))

(defun trees-visible ()
  (let ((sum 0))
    (loop for y from 0
          for row in *matrix* do
            (loop for x from 0
                  for col in row
                  when (visiblep x y) do
                    (incf sum)))
    sum))

(defun direction-scenic-score (dirfn x y item)
  (let ((score 0))
    (block outer
      (funcall dirfn x y #'(lambda (other)
                             (if (< other item)
                                 (incf score)
                                 (return-from outer (incf score)))))
      
      score)))

(defun top-scenic-score (x y item)
  (direction-scenic-score #'foreach-up x y item))

(defun right-scenic-score (x y item)
  (direction-scenic-score #'foreach-right x y item))

(defun bottom-scenic-score (x y item)
  (direction-scenic-score #'foreach-down x y item))

(defun left-scenic-score (x y item)
  (direction-scenic-score #'foreach-left x y item))

(defun scenic-score (x y)
  (let ((item (matrix-get x y *matrix*)))
    (* (top-scenic-score x y item)
       (right-scenic-score x y item)
       (bottom-scenic-score x y item)
       (left-scenic-score x y item))))

(defun highest-scenic-score ()
  (let ((max-score 0))
    (loop for y from 0
          for row in *matrix* do
            (loop for x from 0
                  for col in row do
                  (let ((score (scenic-score x y)))
                    (when (> score max-score) (setf max-score score)))))
    max-score))

(format t "P1: ~a; P2: ~a~%" (trees-visible) (highest-scenic-score))
