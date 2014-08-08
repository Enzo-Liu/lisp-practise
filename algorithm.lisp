;KMP string match
(defun kmp (s w)
  "input is source string and word,this function is to get index for word in source"
  (match_kmp (next w #'char=) s w #'char=))

(defun match_kmp (next s w predicate)
  (labels ((match_kmp_r (next s w m i l wl)
             (if (= (+ m i) l)
               (values l nil)
               (if (funcall predicate (char s (+ m i)) (char w i))
                 (if (= i (1- wl))
                   (values m t)
                   (match_kmp_r next s w m (1+ i) l wl))
                 (if (> (nth i next) -1)
                   (match_kmp_r next s w (- (1+ m) (nth i next)) (nth i next) l wl)
                   (match_kmp_r next s w (1+ m) 0 l wl))))))
    (match_kmp_r next s w 0 0 (length s) (length w))))

(defun next (w predicate)
  (labels ((next_r (next pos cnd w wl)
             (if (= pos wl)
               next
               (cond ((= pos 1)
                      (progn (setf (nth pos next) 0)
                             (next_r next (1+ pos) cnd w wl)))
                     ((funcall predicate (char w (1- pos)) (char w cnd))
                      (progn (setf (nth pos next) (1+ cnd))
                             (next_r next (1+ pos) (1+ cnd) w wl)))
                     ((> cnd 0) (next_r next pos (nth cnd next) w wl))
                     (t (progn (setf (nth pos next) 0)
                               (next_r next (1+ pos) cnd w wl)))))))
    (next_r (make-list (length w) :initial-element -1) 1 0 w (length w))))


;Fibnacci in matrix
;f(n)=f(n-1)+f(n-2) (when n >1)
;f(0)=0,f(1)=1;
(defvar *matrix* '(1 1 1 0))
(defvar *0-matrix* '(1 0 0 1))
(defun fib (n)
  "using 2d-array [1,1,1,0] to calculate fib n,
   reduce this problem from '+' to '*' so that a^n
   can be calculated as a^x1*a^x2*a^xn (x1,x2...xn are all 2^n)"
  (labels (
    (decompose (n) (format nil "~B" n))
    (listMulti (l1 l2) (+ (* (nth 0 l1) (nth 0 l2)) (+ (* (nth 1 l1) (nth 1 l2)))))
    (multi (m1 m2) (list
                     (listMulti (list (nth 0 m1) (nth 1 m1))
                                (list (nth 0 m2) (nth 2 m2)))
                     (listMulti (list (nth 0 m1) (nth 1 m1))
                                (list (nth 1 m2) (nth 3 m2)))
                     (listMulti (list (nth 2 m1) (nth 3 m1))
                                (list (nth 0 m2) (nth 2 m2)))
                     (listMulti (list (nth 2 m1) (nth 3 m1))
                                (list (nth 1 m2) (nth 3 m2)))))
    (multiMatrixBase2 (matrix n value)
                      (cond
                        ((string= value "0") *0-matrix*)
                        ((<= n 1) matrix)
                        (t (multiMatrixBase2 (multi matrix matrix) (1- n) value))))
    (multiMatrix (matrix bits len)
      (if (= 0 len)
        *0-matrix*
        (multi (multiMatrixBase2 matrix len (subseq bits 0 1)) (multiMatrix matrix (subseq bits 1 len) (1- len))))))
    (let ((m (multiMatrix *matrix* (decompose (1- n)) (length (decompose (1- n))))))
      (cond
        ((< n 0) "wrong input")
        ((= n 0) 0)
        (t (nth 0 m))))))


;;HEAP SORT

(defun heapsort (seq)
  seq
  )

