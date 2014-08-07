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

;;HEAP SORT

(defun heapsort (seq)
  seq
  )
