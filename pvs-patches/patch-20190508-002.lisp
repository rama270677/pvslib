;; Reported by P. Masci (NIA) - May 8, 2019
;; The bug is due to the place not being set correctly.  The included patch
;; should fix this.


(in-package :pvs)

(defun xt-name (name &optional name!)
  (let* ((idop (term-arg0 name))
	 (lib (term-arg1 name))
	 (actuals (term-arg2 name))
	 (pidop (case (sim-term-op (term-arg3 name))
		  (NOMOD nil)
		  (PIDACTS (term-arg0 (term-arg3 name)))
		  (t (term-arg3 name))))
	 (dactuals (when (is-sop 'PIDACTS (term-arg3 name))
		     (term-arg1 (term-arg3 name))))
	 (mappings (term-arg4 name))
	 (target (term-arg5 name))
	 (maybe-num? (and (not name!)
			  (is-sop 'NOLIB lib)
			  (is-sop 'NOACTUALS actuals)
			  (is-sop 'NOMAP mappings)
			  (is-sop 'NOTGT target)
			  (or (is-number (term-arg0 idop))
			      (valid-number? (string (ds-id (term-arg0 idop))))))))
    (multiple-value-bind (idops length)
	(xt-name-idops pidop maybe-num?)
      ;; At this point, idops is a number or a symbol (possibly with
      ;; periods), and length is the length of it.  If a number, then we
      ;; know maybe-num? is true, and we can create a rational.  The nil
      ;; symbol will return a length of either nil or 3, depending on whether
      ;; idops is actually empty.
      (assert (or (symbolp idops) (integerp idops)))
      (cond ((integerp idops)
	     (let* ((int-part (if (is-number (term-arg0 idop))
				  (ds-number (term-arg0 idop))
				  (parse-integer (string (ds-id (term-arg0 idop))))))
		    (frac-value idops)
		    (frac-length length))
	       (if (zerop frac-value)
		   (make-instance 'decimal-integer
		     :number int-part
		     :fractional-length frac-length
		     :place (term-place name))
		   (let* ((denom (expt 10 frac-length))
			  (num (+ (* denom int-part) frac-value)))
		     (make-instance 'decimal
		       :operator (mk-name-expr '/)
		       :argument (mk-arg-tuple-expr
				  (make-instance 'number-expr
				    :number num)
				  (make-instance 'number-expr
				    :number denom))
		       :place (term-place name))))))
	    ((and maybe-num? (null length))
	     (multiple-value-bind (num radix)
		 (if (is-number (term-arg0 idop))
		     (ds-number (term-arg0 idop))
		     (parse-number (string (ds-id (term-arg0 idop)))))
	       (assert (integerp num))
	       (if radix
		   (make-instance 'number-expr-with-radix
		     :number num
		     :radix radix
		     :place (term-place name))
		   (make-instance 'number-expr
		     :number num
		     :place (term-place name)))))
	    (t (multiple-value-bind (acts acts-there? dacts dacts-there?)
		   (unless (is-sop 'NOACTUALS actuals)
		     (xt-actuals actuals pidop))
		 (make-instance 'name
		   :id (if (null length)
			   (if (is-number (term-arg0 idop))
			       (makesym "~d" (ds-number (term-arg0 idop)))
			       (let ((id (ds-vid (term-arg0 idop))))
				 (when (memq id '(|/\\| |\\/|))
				   (pushnew id *escaped-operators-used*))
				 id))
			   idops)
		   :library (unless (is-sop 'NOLIB lib)
			      (ds-vid lib))
		   :actuals acts
		   :acts-there? acts-there?
		   :dactuals (or dacts
				 (when dactuals
				   (xt-actuals dactuals t)))
		   :dacts-there? dacts-there?
		   :mappings (unless (is-sop 'NOMAP mappings)
			       (xt-mappings mappings))
		   :mod-id (unless (null length)
			     (if (is-number (term-arg0 idop))
				 (makesym "~d" (ds-number (term-arg0 idop)))
				 (ds-id (term-arg0 idop))))
		   :target (unless (is-sop 'NOTGT target)
			     (xt-modname target))
		   :place (term-place name))))))))
