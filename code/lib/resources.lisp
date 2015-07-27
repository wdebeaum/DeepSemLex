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
       (directory (make-pathname :defaults (base-dir rv)
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
				  :base-dir (make-pathname :directory ,base-dir)
				  :get-files-for-symbol ,get-files-for-symbol
				  ,@(when get-all-files
				      (list :get-all-files get-all-files))
				  )
			      )))
		  )))
	    versions)))
  )

(defmethod print-object ((rv resource-version) s)
  (format s "resource-version ~a" (package-name (pkg rv))))

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

; Note: there is no evict-dsl-file, because by the time we get down to the file
; level, we don't know which concepts we're talking about

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

;; FIXME I think I actually have to do this a different way, since the types
;; concept and (or symbol concept) are not the same, so I can't just replace
;; concepts with their names. I was thinking that was what I did while loading,
;; but actually what I do is create stub concepts and then change their class
;; (!) and add slots when they're actually loaded.
(defun evict-concept (concept)
  "Roughly speaking, undo the effect of require-concept. Replace concept with
   its name in all its references, and remove it from the *db*, so that it can
   be garbage-collected. Also update *loaded-concept-names* and
   *loaded-resource-files* so that we can still load the concept back in later
   if we need to."
  (let* ((name (name concept))
         (rv (gethash (symbol-package name) *resource-versions*)))
    ;; replace concept with name in all references
    (dolist (ref (references concept))
      (etypecase ref
        (cons
	  (unless (eq concept (car ref)) (error "WTF"))
	  (rplaca ref name))
	(standard-object
	  (loop for slot-def in (class-slots (class-of ref))
	        for slot-name = (slot-definition-name slot-def)
	        when (eq concept (slot-value ref slot-name))
	          do (setf (slot-value ref slot-name) name)))
	))
    ;; remove from *db*
    (remhash name (concepts *db*))
    (when (typep concept 'sense)
      (remove-morphed-sense-from-db *db* concept))
    ;; remove from *loaded-concept-names*
    (remhash name *loaded-concept-names*)
    ;; remove from *loaded-resource-files* if applicable
    (when (and rv (get-files-for-symbol rv))
      (let ((files (funcall (get-files-for-symbol rv) rv name)))
        (dolist (file files)
	  (remhash file *loaded-resource-files*))))
    ))

(defun require-resource-version (pkg-name)
  "Ensure that all files from the resource version identified by the package
   name are loaded."
  (let* ((pkg (find-package pkg-name))
         (rv (gethash pkg *resource-versions*))
	 (files (funcall (get-all-files rv) rv)))
    (when *load-verbose*
      (format *standard-output* "; requiring ~s files from ~s~%" (length files) rv))
      (let ((*load-verbose* nil))
	(dolist (f files)
	  (require-dsl-file f)))))

(defun evict-resource-version (pkg-name)
  "Roughly speaking, undo the effect of require-resource-version. Evict all of
   the concepts in the named resource verson (really those whose names are
   symbols in the corresponding Lisp package)."
  (loop with pkg = (find-package pkg-name)
        for k being the hash-keys of (concepts *db*)
        when (eq pkg (symbol-package k))
	  ; note: can't just do evict-concept here, since it modifies the hash
	  collect k into to-be-evicted
	finally
	  (dolist (k to-be-evicted)
	    (evict-concept (gethash k (concepts *db*))))
	))

(defun require-all-resource-files ()
  "Ensure that all files from all resource versions with files to load are
   loaded."
  (maphash
      (lambda (pkg rv)
        (dolist (f (funcall (get-all-files rv) rv))
	  (load-dsl-file f)))
      *resource-versions*))

(defresource (ONT)
  ( :base-dir (pathname-directory #!TRIPS"src;OntologyManager;Data;LFdata;*")
    :get-all-files
    (lambda (rv)
      (let ((bd (base-dir rv)))
	(mapcar
	    (lambda (s)
	      (make-pathname :defaults bd :name s :type "lisp"))
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

;;; FIXME there's got to be a better way to load templates and senses...
(defresource (ONT-T)
  ( :base-dir (pathname-directory #!TRIPS"src;LexiconManager;Data;templates;*")
    ))

(defresource (ONT-W)
  ( :base-dir (pathname-directory #!TRIPS"src;LexiconManager;Data;new;*") ))

(defun data-subdir (subdir-str)
  ;(pathname-directory (trips::make-trips-pathname (concatenate 'string "src;DeepSemLex;data;" subdir-str)))
  (pathname-directory (trips::make-trips-pathname (concatenate 'string "..;data;" subdir-str)))
  )

(defresource (WN WordNet)
  ;; the latest downloadable version
  ;; FIXME can actually download just the database files for 3.1
  ( :version "3.0" #|| :base-dir (data-subdir "WordNet;*")
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
  ( :version "3.2" :base-dir (data-subdir "VerbNet;*")
    :get-files-for-symbol (lambda (rv sym)
      (let* ((name (string-downcase (symbol-name sym)))
	     (probed (probe-file (make-pathname :defaults (base-dir rv)
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
		(directory (make-pathname :defaults (base-dir rv)
		                          :name :wild :type "lisp"))
		)
	      )
	    )))
    ))

(defresource (PB PropBank)
  ( :base-dir (data-subdir "PropBank;frames;*")
    :get-files-for-symbol (lambda (rv sym)
      (let* ((name (string-downcase (symbol-name sym)))
             (dot-pos (or (position #\. name) 0))
	     (word (subseq name 0 dot-pos)))
        (when (string= "" word)
	  (error "bogus PropBank frame ID: ~s" sym))
	(directory (make-pathname :defaults (base-dir rv)
	                          :name word :type "lisp"))))
    ))

(defresource (ON OntoNotes)
  ( :version "3.0"
    :base-dir (data-subdir "OntoNotes;sense-inventories;*")
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
        (directory (make-pathname :defaults (base-dir rv)
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

