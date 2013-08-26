(in-package :dsl)

(defclass-simple resource-version ()
  "A (version of a) resource containing named concepts."
  (package pkg
   "The package all the names of concepts in this resource version are in.")
  ((maybe string) version
   "The version number of the resource as a string."
   nil)
  ((maybe pathname) base-dir
   "The directory the lisp files in this resource version are under."
   nil)
  ((maybe (function (resource-version symbol) (list-of pathname)))
   get-files-for-symbol
   "A function that, given this instance and a symbol in pkg, returns a list
    of pathnames for existing files that are required for a complete
    definition of the concept the symbol names."
   nil)
  ((function (resource-version) (list-of pathname))
   get-all-files
   "A function that, given this instance, returns a list of pathnames for
    existing files that are required for a complete definition of all concepts
    in this resource version."
   (lambda (rv)
     (when (base-dir rv)
       (directory (make-pathname :directory (base-dir rv)
                                 :name :wild :type "lisp"))))
   )
  )

(defvar *resource-versions* (make-hash-table :test #'eq) "Hash table from resource version packages to resource-version class instances.")

(defmacro defresource (names &rest versions)
  "Make a set of resource-version instances and their corresponding packages."
  (unless (listp names)
    (setf names (list names)))
  (unless versions
    (setf versions '(nil)))
  `(progn
    ,@(let ((first-version t))
	(mapcar
	    (lambda (version-spec)
	      (destructuring-bind (&key version base-dir get-files-for-symbol
	                           get-all-files)
	          version-spec
	        (let ((versioned-names (when version (mapcar (lambda (n) (concatenate 'string (string n) "-" (string version))) names))))
	          (when first-version
		    (setf versioned-names (append names versioned-names))
	            (setf first-version nil)
		    )
		  (when versioned-names
		    `(let ((new-pkg (defpackage ,(car versioned-names)
				      (:use)
				      (:nicknames ,@(cdr versioned-names)))))
			(setf (gethash new-pkg *resource-versions*)
			      (make-instance 'resource-version
				  :pkg new-pkg
				  :version ,version
				  :base-dir ',base-dir
				  :get-files-for-symbol ,get-files-for-symbol
				  ,@(when get-all-files
				      (list :get-all-files get-all-files))
				  )
			      )))
		  )))
	    versions)))
  )

(defvar *loaded-resource-files* (make-hash-table :test #'equal)
 "A hash whose keys are the resource files we've already loaded.")
(defvar *loaded-concept-names* (make-hash-table :test #'eq)
 "A hash whose keys are the names of concepts we've already loaded from
  resources.")

(defun require-dsl-file (file)
  "Ensure that the given file is loaded."
  (unless (gethash file *loaded-resource-files*)
    (load-dsl-file file)
    (setf (gethash file *loaded-resource-files*) t)
    ))

(defun require-concept (name)
  "Ensure that the named concept is completely defined according to the
   resource of the name symbol's package."
  (unless (gethash name *loaded-concept-names*)
    (let ((rv (gethash (symbol-package name) *resource-versions*)))
      (when (and rv (get-files-for-symbol rv))
        (let ((files (funcall (get-files-for-symbol rv) rv name)))
	  (dolist (file files)
	    (require-dsl-file file)))))
    (setf (gethash name *loaded-concept-names*) t)
    ))

(defun require-resource-version (pkg-name)
  "Ensure that all files from the resource version identified by the package
   name are loaded."
  (let* ((pkg (find-package pkg-name))
         (rv (gethash pkg *resource-versions*)))
    (dolist (f (funcall (get-all-files rv) rv))
      (require-dsl-file f))))

(defun require-all-resource-files ()
  "Ensure that all files from all resource versions with files to load are
   loaded."
  (maphash
      (lambda (pkg rv)
        (dolist (f (funcall (get-all-files rv) rv))
	  (load-dsl-file f)))
      *resource-versions*))

(defresource (ONT)
  ( :base-dir (:relative :up :up :up "OntologyManager" "Data" "LFdata")
    :get-all-files
    (lambda (rv)
      (let ((bd (base-dir rv)))
	(mapcar
	    (lambda (s)
	      (make-pathname :directory bd :name s :type "lisp"))
	    '("abstract-types"
	      "physobj"
	      "predicates"
	      "root-types"
	      "situation-types"
	      "specific-situation-types"
	      "speech-acts"
	      "time-location-types"
	      )
	    )))
    ))

(defresource (WN WordNet)
  ;; the latest downloadable version
  ;; FIXME can actually download just the database files for 3.1
  ( :version "3.0" #|| :base-dir (:relative :up :up "data" "WordNet")
    :get-files-for-symbol (lambda (rv sym)
      ;; TODO
      ) ||#
    )
  ;; the version currently searchable on the WN website
  (:version "3.1")
  ;; versions mentioned in OntoNotes
  (:version "2.2")
  (:version "2.1")
  (:version "2.0")
  (:version "1.7")
  (:version "1.2")
  (:version "1.0")
  )

(defresource (VN VerbNet)
  ( :version "3.2" :base-dir (:relative :up :up "data" "VerbNet")
    :get-files-for-symbol (lambda (rv sym)
      (let* ((name (string-downcase (symbol-name sym)))
	     (probed (probe-file (make-pathname :directory (base-dir rv)
				                :name name :type "lisp"))))
	(or (when probed (list probed))
	    (let* ((first-dash-pos (or (position #\- name) 0))
	           (second-dash-pos
		     (or (position #\- name :start (1+ first-dash-pos))
		         (length name)))
		   (english-name (subseq name 0 first-dash-pos))
		   (numeric-name (subseq name (1+ first-dash-pos)
		                         second-dash-pos))
		   ; between second-dash-pos and the end of the string is the
		   ; child ID, which we don't need
		   )
	      ;; get $base-dir/*-$numeric-name.lisp
	      (remove-if-not
	        (lambda (pn)
		  (let* ((found-name (pathname-name pn))
		         (found-dash-pos (or (position #\- found-name) 0))
			 (found-numeric-name
			   (subseq found-name (1+ found-dash-pos)))
			 )
		    (string= found-numeric-name numeric-name)))
		(directory (make-pathname :directory (base-dir rv)
		                          :name :wild :type "lisp"))
		)
	      )
	    )))
    ))

(defresource (PB PropBank)
  ( :base-dir (:relative :up :up "data" "PropBank" "frames")
    :get-files-for-symbol (lambda (rv sym)
      (let* ((name (string-downcase (symbol-name sym)))
             (dot-pos (or (position #\. name) 0))
	     (word (subseq name 0 dot-pos)))
        (when (string= "" word)
	  (error "bogus PropBank frame ID: ~s" sym))
	(directory (make-pathname :directory (base-dir rv)
	                          :name word :type "lisp"))))
    ))

(defresource (ON OntoNotes)
  ( :version "3.0"
    :base-dir (:relative :up :up "data" "OntoNotes" "sense-inventories")
    :get-files-for-symbol (lambda (rv sym)
      (let* ((name (string-downcase (symbol-name sym)))
             (first-dot-pos (or (position #\. name) 0))
	     (second-dot-pos (or (position #\. name :start (1+ first-dot-pos))
	                         0))
	     (word (subseq name 0 first-dot-pos))
	     (pos (subseq name (1+ first-dot-pos) second-dot-pos))
	     ; after the second dot is the sense number, but we don't need that
	     (filename (concatenate 'string word "-" pos))
	     )
        (directory (make-pathname :directory (base-dir rv)
	                          :name filename :type "lisp"))))
    ))

;; random stuff mentioned in OntoNotes
(defresource (MWO Merriam-Webster_Online))
(defresource MAC)
(defresource answers.com)

(defresource (FN FrameNet)) ; TODO

;; We don't actually need this because SemLink doesn't define any concepts of
;; its own.
; (defresource (SL SemLink))

