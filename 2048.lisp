;;this can be customerized,from 0 index
(defvar *height* 3)
(defvar *width* 3)
(defvar *values* (cons 4 (make-list 9 :initial-element 2)))
(defvar *won-value* 2048)

(defclass tile ()
  ((value :initform 0 :initarg :value :accessor value)))

(defclass world ()
  ((board :accessor board)))

(defmacro board-loop ((row a b) (col c d) form &body body)
  `(loop for row from ,a to ,b ,form
        (loop for col from ,c to ,d ,form
             (progn ,@body))))

(defun index (l row col)
  (nth col (nth row l)))

(defmacro bval (r c)
  `(value (index (board this) ,r ,c)))

(defmethod initialize-instance :after ((this world) &key)
  (setf (board this)
        (board-loop (row 0 *height*) (col 0 *width*) collect
          (make-instance 'tile :value 0))))

(defmethod left ((this world))
  (flet ((shift (var)
           (loop for 0-index from 0 to *width*
              when (= 0 (bval var 0-index)) do
                (loop for !0-index from (1+ 0-index) to *width*
                   when (not (= 0 (bval var !0-index)))  do
                     (progn (setf (bval var 0-index) (bval var !0-index))
                            (setf (bval var !0-index) 0)
                            (return)))))
         (combine (var)
           (loop for cur from 0 to (1- *width*)
              when (= (bval var cur) (bval var (1+ cur))) do
                (progn (setf (bval var cur) (* 2 (bval var cur)))
                       (setf (bval var (1+ cur)) 0)))))
    (dotimes (i (1+ *height*)) (shift i) (combine i) (shift i))))

;;;this function rotates the board by 90 degrees counter-clockwise
(defmethod rotate ((this world))
  (setf (board this) (reverse (apply #'mapcar #'list (board this)))))

;;;to move right, rotate twice, left, rotate twice
(defmethod right ((this world))
  (progn
    (dotimes (i 2) (rotate this))
    (left this)
    (dotimes (i 2) (rotate this))))

;;;to move up, rotate once, left, rotate 3 times
(defmethod up ((this world))
  (progn
    (rotate this)
    (left this)
    (dotimes (i 3) (rotate this))))

;;;to move down rotate three times, left, rotate once
(defmethod down ((this world))
  (progn
    (dotimes (i 3) (rotate this))
    (left this)
    (rotate this)))

(defmethod not-fullp ((this world))
  (board-loop (row 0 *height*) (col 0 *width*) thereis (= 0 (bval row col))))

(defmethod fullp ((this world)) (not (not-fullp this)))

(defmethod random-insert ((this world))
  (if (not-fullp this)
      (let ((v (elt *values* (random (length *values*))))
            (r (random (1+ *height*)))
            (c (random (1+ *width*))))
        (do () ((= 0 (bval r c)))
          (setf r (random (1+ *height*)))
          (setf c (random (1+ *width*))))
        (setf (bval r c) v))))

(defmethod lostp ((this world))
  (not
   (or
    (board-loop (row 0 *height*) (col 0 (1- *width*)) thereis
      (= (bval row col) (bval row (+ col 1))))
    (board-loop (row 0 (1- *height*)) (col 0 *width*) thereis
      (= (bval row col) (bval (+ row 1) col)))
    (not-fullp this))))

;;; this method returns whether the game is won
(defmethod wonp ((this world))
  (board-loop (row 0 *height*) (col 0 *width*) thereis
    (= (bval row col) *won-value*)))

(defmethod p-world ((this world))
  (board-loop (row 0 *height*) (col 0 *width*) do (princ (bval row col)))
  (format t "~%"))

(defvar *world* (make-instance 'world))
