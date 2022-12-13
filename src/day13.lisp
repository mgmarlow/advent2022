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

;; TODO: Revisit recursive solution
;; (defun validate-pair (l1 l2)
;;   (loop for a in l1
;;         for b in l2 do
;;           (block iteration
;;             (cond
;;               ((and (null a) (not (null b))) t)
;;               ((and (not (null a)) (null b)) nil)
;;               ((and (null a) (null b)) (return-from iteration))
;;               ((and (integerp a) (integerp b)) (cond
;;                                                  ((= a b) (return-from iteration))
;;                                                  (t (< a b))))
;;               ((and (listp a) (listp b)) (validate-pair a b))
;;               ((integerp a) (validate-pair (list a) b))
;;               ((integerp b) (validate-pair a (list b)))))))

(defun validate-pairs ()
  (loop for pair in *input-pairs*
        for i from 1
        if (validate-pair (first pair) (second pair))
          summing i into validated
        end
        finally (return validated)))
