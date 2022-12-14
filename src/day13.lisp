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
    ((and (null left) (null right)) 'continue)
    ((and (null left) (not (null right))) 'valid)
    ((and (not (null left)) (null right)) 'invalid)
    ((and (integerp left) (integerp right))
     (cond ((= left right) 'continue)
           ((< left right) 'valid)
           (t 'invalid)))
    ((and (listp left) (listp right))
     (progn (loop for lprime in left
                  for rprime in right do
                    (let ((rst (rvalid lprime rprime)))
                      (unless (equal rst 'continue)
                        (return-from rvalid rst)))))
     (cond
       ((= (length left) (length right)) 'continue)
       (t (if (< (length left) (length right)) 'valid 'invalid))))
    ((and (listp left) (integerp right))
     (rvalid left (list right)))
    ((and (integerp left) (listp right))
     (rvalid (list left) right))
    (t 'valid)))

(defun validate-pair (l1 l2)
  (loop for left in l1
        for right in l2 do
          (let ((rst (rvalid left right)))
            (unless (equal rst 'continue)
              (return-from validate-pair rst))))
  (if (< (length l1) (length l2)) 'valid 'invalid))

(defun valid-pair (l1 l2)
  (equal (validate-pair l1 l2) 'valid))

(defun validate-pairs ()
  (loop for pair in *input-pairs*
        for i from 1
        if (valid-pair (first pair) (second pair))
          summing i into validated
        end
        finally (return validated)))
