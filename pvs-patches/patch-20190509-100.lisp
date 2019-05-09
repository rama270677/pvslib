;; Fix for theory interpretation bug reported by mariano.m.moscato@nasa.gov on 9 Apr 2019
;; The context created for the mapped-axiom TCC was including the axiom itself,

(in-package :pvs)

(defmethod update-context-importing-for-mapped-tcc ((decl mapped-axiom-tcc))
  "The context for mapped-axiom-tccs is special, as it should not be able to
prove itself from the mapped axioms."
  (assert (theory-instance decl))
  (assert *current-context*)
  (let* ((thname (theory-instance decl))
	 (th (get-theory thname))
	 (thdecls (all-decls th))
	 (post-decls (memq (generating-axiom decl) thdecls))
	 (prev-decls (ldiff thdecls post-decls))
	 (*insert-add-decl* nil))
    ;;; Want something like add-usings-to-context*, but only for those
    ;;; importings that precede the given declaration.
    (add-preceding-importings prev-decls th thname)))
