(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up :up "config" "lisp")
                       :name "trips")))
(load #!TRIPS"src;DeepSemLex;code;converters;dsl-to-xml")

(in-package :dsl)

(defun cl-user::run (output-dir)
  (require-resource-version :ont)
  (require-resource-version :ont-t)
  (require-resource-version :ont-w)
  (let ((*package* (find-package :dsl))
	(ont-pkg (find-package :ont)))
    (maphash
      (lambda (concept-name concept)
	(when (and (eq ont-pkg (symbol-package concept-name)) ;; ONT::*
		   ;; was actually defined in TRIPS
		   (provenance concept)
		   (eq 'TRIPS (name (car (provenance concept)))))
	  (with-open-file
	    (
	      xml
	      (format nil "~a/ONT::~a.xml" output-dir
		      (string-downcase (symbol-name concept-name)))
	      :direction :output
	      :if-exists :supersede
	    )
	    (format xml "<?xml version=\"1.0\"?>~&<?xml-stylesheet type=\"text/xsl\" href=\"ont-type.xsl\"?>~&<dsl>")
	    ;; set and write the provenance so we don't keep repeating it
	    (let ((*current-provenance* (car (provenance concept))))
	      (dsl-to-xml-stream (listify *current-provenance*) xml)
	      ;; write the main concept,
	      (dsl-to-xml-stream (listify concept) xml)
	      ;; ... its ancestors, 
	      (dolist (anc (eval-path-expression '(+ >inherit) (list concept)))
		(dsl-to-xml-stream (listify anc) xml))
	      ;; ... and its children (including senses)
	      (dolist (child (eval-path-expression '<inherit (list concept)))
	        ;; TODO include non-parametric template definitions?
		(dsl-to-xml-stream (listify child) xml))
	      (format xml "~&</dsl>~%")
	      )
	    )))
      (concepts *db*)
      )
    ;; TODO W::*.xml
    )
  )
