(defun convert-to-list (line)
  (let ((rst))
    (setf rst (substitute #\Space #\, line))
    (setf rst (substitute #\( #\[ rst))
    (setf rst (substitute #\) #\] rst))
    (read-from-string rst)))

(defparameter *input-pairs*
  (with-open-file (in "../data/day13.txt")
    (let* ((lists (loop for line = (read-line in nil)
                        while line
                        when (> (length line) 0)
                          collect (convert-to-list line)))
           ;; TODO: This can be cleaned up
           (pairs (loop for lst in lists
                         for i from 0
                         if (evenp i)
                           collect lst into evens
                         else
                           collect lst into odds
                         end
                         finally (return (list evens odds)))))
      (reduce #'(lambda (a b) (mapcar 'list a b)) pairs))))


(defun rvalid (left right)
  (cond
    ((and (integerp left) (integerp right) (< left right)) 1)
    ((and (integerp left) (integerp right) (> left right)) 0)
    ((and (integerp left) (integerp right) (= left right)) -1)
    ((and (null left) (listp right)) 1)
    ((and (listp left) (null right)) 0)
    ((and (null left) (null right)) -1)
    ((and (integerp left) (listp right)) (rvalid (list left) right))
    ((and (listp left) (integerp right)) (rvalid left (list right)))
    (t (let ((rst (rvalid (first left) (first right))))
         (if (= rst -1)
             (rvalid (rest left) (rest right))
             rst)))))

(defun valid-pair (l1 l2)
  (= (rvalid l1 l2) 1))

(defun validate-pairs ()
  (loop for pair in *input-pairs*
        for i from 1
        if (valid-pair (first pair) (second pair))
          summing i into validated
        end
        finally (return validated)))
