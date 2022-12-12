(defconstant +start+ #\S)
(defconstant +end+ #\E)

(defun elevation (char)
  (position char "abcdefghijklmnopqrstuvwxyz"))

(defclass point ()
  ((x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)))

(defclass heightmap ()
  ((contents
    :initarg :contents
    :accessor contents)
   (current-pos
    :initarg :current-pos
    :accessor current-pos)
   (endpoint
    :initarg :endpoint
    :accessor endpoint)))

(defmethod width ((m heightmap))
  ;; Assuming every row is the same length
  (length (first (contents m))))

(defmethod height ((m heightmap))
  (length (contents m)))

(defmethod at ((m heightmap) p)
  (nth (x p) (nth (y p) (contents m))))

;; 0,0 = top-left
(defun find-point (map-contents item)
  (loop for y from 0
        for row in map-contents do
          (loop for x from 0
                for col in row do
                  (when (char= col item)
                    (return-from find-point
                      (make-instance 'point :x x :y y))))))

(defun expand-string (str)
  (loop for i across str collect i))

(defun new-map ()
  (let ((contents
           (with-open-file (in "../data/day12.txt")
             (loop for line = (read-line in nil)
                   while line
                   when (> (length line) 0)
                     collect (expand-string line)))))
    (make-instance 'heightmap
                   :contents contents
                   :current-pos (find-point contents +start+)
                   :endpoint (find-point contents +end+))))

(defparameter *heightmap* (new-map))

(defmethod valid-move ((m heightmap) dest)
  (cond
    ((char= (at m (current-pos m)) +start+) t)
    ((char= (at m (current-pos m)) +end+) t)
    (t (<= (abs (- (elevation (at m dest))
                   (elevation (at m (current-pos m)))))
           1))))

;; todo: actual algorithm :p
