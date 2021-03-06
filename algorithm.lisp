;;; algorithm.lisp ---
;;
;; Filename: algorithm.lisp
;; Description:
;; Author: Liu Enze
;; Maintainer:
;; Created: Fri Nov 28 07:49:16 2014 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Fri Nov 28 07:49:16 2014 (+0800)
;;           By: Liu Enze
;;     Update #: 1
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;;; algorithm.lisp ---
;;
;; Filename: algorithm.lisp
;; Description:
;; Author: Liu Enze
;; Maintainer:
;; Created: Fri Nov 28 07:49:16 2014 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Fri Nov 28 07:49:16 2014 (+0800)
;;           By: Liu Enze
;;     Update #: 1
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


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

;gcd
(defun gcd_m (a b)
  "using gcd(x,y) = gcd (x-y,y)"
  (if (= 0 b)
    a
    (gcd b (mod a b))))

;;QUICK SORT
(defun quicksort (seq)
  (if (>= 1 (length seq)) seq
      (let* ((key (first seq))
             (less (remove-if-not (lambda (x) (< x key)) seq))
             (more (remove-if-not (lambda (x) (> x key)) seq))
             (same (remove-if-not (lambda (x) (= x key)) seq)))
        (append (quicksort less) same (quicksort more)))))

(defun quicksort2 (seq)
  (if (>= 1 (length seq)) seq
      (labels ((partition (seq key)
                 (loop for x in seq
                    when (< x key) collect x into less
                    when (= x key) collect x into same
                    when (> x key) collect x into more
                    finally (return (values less same more)))))
        (multiple-value-bind (less same more) (partition seq (first seq))
          (append (quicksort2 less) same (quicksort2 more))))))

;;HEAP SORT
(defun heapsort (a &optional (count (length a)))
  (macrolet ((ref (i) `(aref a ,i))
             (swap (i j) `(rotatef (ref ,i) (ref ,j)))
             (ref< (i j) `(< (ref ,i) (ref ,j))))
    (labels ((sift (root count)
               (let ((c1 (+ (* 2 root) 1))
                     (c2 (+ (* 2 root) 2)))
                 (when (< c1 count)
                   (let ((c (if (and (< c2 count) (ref< c1 c2)) c2 c1)))
                     (when (ref< root c)
                       (swap root c)
                       (sift c count)))))))
      (loop for start from (1- (floor count 2)) downto 0
         do (sift start count))
      (loop for end from (1- count) downto 1
         do (swap 0 end) (sift 0 end))))
  a)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; algorithm.lisp ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; algorithm.lisp ends here
