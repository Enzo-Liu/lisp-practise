;;; lecture 5a
;;; mente-carlo the calcuate pai

(defconstant +LIMIT+ 100000000)

(defun menta-carlo (trials fn)
  (/ (loop for i below trials
        counting (funcall fn) into passed
        finally (return passed)) trials))

(defun pai-test ()
  (= 1 (gcd (random +LIMIT+) (random +LIMIT+))))

(sqrt (/ 6 (menta-carlo 10000000 #'pai-test) ))
