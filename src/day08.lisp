(defparameter *matrix*
  (with-open-file (in "../data/day08.txt")
    (loop for line = (read-line in nil)
          while line
          when (> (length line) 0)
            collect (map 'list #'digit-char-p line))))

(defmacro matrix-get (x y matrix)
  `(nth ,x (nth ,y ,matrix)))

(defun top-visiblep (x y item)
  (loop for i from 0 to (1- y) do
    (let ((other (matrix-get x i *matrix*)))
      (if (>= other item)
          (return-from top-visiblep nil))))
  t)

(defun right-visiblep (x y item)
  (loop for i from (1+ x) to (1- (length (first *matrix*))) do
    (let ((other (matrix-get i y *matrix*)))
      (if (>= other item)
          (return-from right-visiblep nil))))
  t)

(defun bottom-visiblep (x y item)
  (loop for i from (1+ y) to (1- (length *matrix*)) do
    (let ((other (matrix-get x i *matrix*)))
      (if (>= other item)
          (return-from bottom-visiblep nil))))
  t)

(defun left-visiblep (x y item)
  (loop for i from 0 to (1- x) do
    (let ((other (matrix-get i y *matrix*)))
      (if (>= other item)
          (return-from left-visiblep nil))))
  t)

(defun search-visiblep (x y)
  (let ((dirs '()) (item (matrix-get x y *matrix*)))
    (when (top-visiblep x y item) (push 'top dirs))
    (when (right-visiblep x y item) (push 'right dirs))
    (when (bottom-visiblep x y item) (push 'bottom dirs))
    (when (left-visiblep x y item) (push 'left dirs))
    (> (length dirs) 0)))

(defun visiblep (x y)
  (cond
    ((= x 0) t)
    ((= x (1- (length (first *matrix*)))) t)
    ((= y 0) t)
    ((= y (1- (length *matrix*))) t)
    (t (search-visiblep x y))))

(defun trees-visible ()
  (let ((sum 0))
    (loop for y from 0
          for row in *matrix* do
            (loop for x from 0
                  for col in row
                  when (visiblep x y) do
                    (incf sum)))
    sum))
