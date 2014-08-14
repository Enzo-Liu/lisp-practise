(defclass person ()
  ((name :accessor person-name
         :initform 'bill
         :initarg :name)
   (age :accessor person-age
        :initform 10
        :initarg :age)))

(defun make-person (name age)
  (make-instance 'person :name name :age age))
