(in-package "CL-USER")
;; mini-clog-pkg-exports: export all bound syms of mini-clog this file
;; should be loaded after all the loading the last of the files
;; implementing mini-clog
(in-package "MINI-CLOG")

(defvar $exports
  (remove-if-not (lambda (x)
		   (and (or (boundp x) (fboundp x) (find-class x nil))
			(not (find (elt (symbol-name x) 0) "$"))))
		 (let ((package "MINI-CLOG") ret)
		   (assert (setq package (find-package package)))
		   (do-symbols (x package)
		     (let ((str (symbol-name x)))
		       (multiple-value-bind (sym state)
			   (find-symbol str package)
			 (when (and sym (eql state :internal)
				    (eql (symbol-package sym) package))
			   (push sym ret)))))
		   (sort ret 'string< :key 'symbol-name))))

(export $exports "MINI-CLOG")
