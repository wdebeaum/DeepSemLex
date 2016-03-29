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
		    `(let ((new-pkg (def-or-update-package
				      ,(car versioned-names)
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
 "A hash whose keys are the resource files we've already loaded, and whose
  values are the value of *require-file-count* the last time the file was
  required.")
(defvar *permanently-loaded-resource-files* (make-hash-table :test #'equal)
 "A hash whose keys are the resource files we've loaded permanently, so that
  they won't be evicted by maybe-evict-dsl-files.")
(defvar *loaded-concept-names* (make-hash-table :test #'eq)
 "A hash whose keys are the names of concepts we've already loaded from
  resources.")
(defvar *require-file-count* 0
 "The number of times any DSL file has been required.")
(defvar *max-loaded-resource-files* 20000
 "The maximum number of resource files to have loaded into memory at one time.
  Set to nil to disable eviction.")
(defvar *num-files-to-evict* 1000
 "The number of files to evict when *max-loaded-resource-files* is reached.")

(defun maybe-evict-dsl-files ()
  "Evict *num-files-to-evict* if *max-loaded-resource-files* has been reached."
  ;; if we have the maximum number of files loaded
  (when (and *max-loaded-resource-files*
	     (<= *max-loaded-resource-files*
	         (hash-table-count *loaded-resource-files*)))
    (format t "; *max-loaded-resource-files*=~s reached; evicting ~s files"
            *max-loaded-resource-files* *num-files-to-evict*)
    ;; find some least-recently-required files
    (loop with to-evict = nil
          for k being the hash-keys of *loaded-resource-files*
          for v = (gethash k *loaded-resource-files*)
	  unless (gethash k *permanently-loaded-resource-files*)
	  do
	    ;; insert (k . v) into to-evict in order of descending v (i.e.
	    ;; least recently required first)
	    (setf to-evict
	          (merge 'list (list (cons k v)) to-evict #'> :key #'cdr))
	    ;; keep to-evict length down
	    (when (< *num-files-to-evict* (length to-evict))
	      (pop to-evict))
	  finally
	    ;; evict each file
	    ;; FIXME it might be more efficient to push this into
	    ;; evict-dsl-file, making it evict-dsl-fileS; if their concept
	    ;; graphs overlap we can avoid having to erase the boundaries
	    ;; between them, and regardless we can avoid scanning the whole
	    ;; database multiple times for concepts/senses to evict
	    (dolist (p to-evict) (evict-dsl-file (car p)))
	  )))

(defun require-dsl-file (file &key provenance-name permanently)
  "Ensure that the given file is loaded."
  ;; if we don't have the file loaded
  (unless (gethash (namestring file) *loaded-resource-files*)
    ;; make sure there's room
    (maybe-evict-dsl-files)
    ;; load the required file
    (load-dsl-file file :provenance-name provenance-name)
    )
  ;; remember that we required the file so we don't evict it too soon
  (setf (gethash (namestring file) *loaded-resource-files*)
	(incf *require-file-count*))
  (when permanently
    (setf (gethash (namestring file) *permanently-loaded-resource-files*) t))
  )

(defun provenance-from-file-p (p f)
     (declare (type provenance p)
              (type string f))
  (and (stringp (filename p))
       (string= (filename p) f)))

(defun object-could-be-from-file-p (o f)
    (declare (type string f))
  "Could o have been created while loading the DSL file named f?"
  (typecase o
    (concept
      (and (provenance o)
	   (every
	     (lambda (p)
	       (provenance-from-file-p p f))
	     (provenance o))))
    ((or relation input-text)
      (and (provenance o)
	   (provenance-from-file-p (provenance o) f)))
    (otherwise
      t)))

(defun traverse-stuff-from-file (filename start &optional (traversed (make-hash-table :test #'eq)) prev)
    (declare (type string filename)
             (type hash-table traversed))
  "Traverse objects starting at start that were created when the named DSL file
   was loaded. Put traversed objects in the keys of the traversed hash-table
   (which should have :test eq). Return a list of (traversed . not-traversed)
   pairs, where not-traversed would have been traversed if it were from the DSL
   file."
  (when (gethash start traversed)
    (return-from traverse-stuff-from-file nil))
  (typecase start
    (cons ; unfortunately, conses aren't standard-objects
      (setf (gethash start traversed) t)
      (append (traverse-stuff-from-file filename (car start) traversed start)
              (traverse-stuff-from-file filename (cdr start) traversed start)))
    (standard-object
      (unless (object-could-be-from-file-p start filename)
	; start is not from file, stop here and return boundary pair
        (return-from traverse-stuff-from-file
	  (when prev
	    (list (cons prev start)))))
      ; start is from file, recurse on all bound slots
      (setf (gethash start traversed) t)
      (loop for slot-def in (class-slots (class-of start))
	    for slot-name = (slot-definition-name slot-def)
	    unless (eq 'provenance slot-name)
	    when (slot-boundp start slot-name)
	    append (traverse-stuff-from-file
		       filename (slot-value start slot-name) traversed start)
	    ))
    (otherwise
      (setf (gethash start traversed) t)
      nil)
    ))

(defun evict-dsl-file (file)
    (declare (type (or pathname string) file))
  "Look for named concepts whose provenance has the given filename, and delete
   them and any anonymous concepts or non-concepts (relations, input-texts)
   connected to them with the same provenance, if it's their only provenance."
  (let* (
         ;; get all named concepts with *some* parts from file
         (named-concepts-from-file
	    (loop for concept-name being the hash-keys of (concepts *db*)
		  for concept = (gethash concept-name (concepts *db*))
		  when (some (lambda (p)
			       (provenance-from-file-p p (namestring file)))
			     (provenance concept))
		  collect concept))
	 ;; similarly, get all senses with some parts from file
	 (senses-from-file ;; FIXME this is expensive
	    (loop for word being the hash-keys of (senses *db*)
	          for senses = (gethash word (senses *db*))
		  append (remove-if-not
		           (lambda (s)
			     (some
			       (lambda (p)
			         (provenance-from-file-p p (namestring file)))
			       (provenance s)))
			   senses)))
	 ;; put the two sets together
	 (named-concepts-and-senses-from-file
	   (remove-duplicates (append named-concepts-from-file senses-from-file)
			      :test #'eq))
	 ;; find all the connected objects that are *only* from the file (they
	 ;; don't have parts loaded from other files), and the boundary of that
	 ;; set with the rest of the DB (not counting the start set)
         (traversed (make-hash-table :test #'eq))
	 (boundary-pairs
	   (loop for start in named-concepts-and-senses-from-file
	         append (traverse-stuff-from-file file start traversed)))
	 )
    ;; remove boundary relations
    (loop for (inside . outside) in boundary-pairs
          when (and (typep inside 'relation) (typep outside 'concept))
	  do (remove-relation inside))
    ;; for named concepts and senses that are only from the file:
    (loop for c in named-concepts-and-senses-from-file
          when (object-could-be-from-file-p c (namestring file))
	  do ;; remove traversed relations
	     (dolist (r (append (in c) (out c)))
	       (when (gethash r traversed)
	         (remove-relation r)))
	     ;; if it's a sense, remove it from (senses *db*)
	     (when (typep c 'sense)
	       (remove-morphed-sense-from-db *db* c))
	     ;; if it's named, mark it as no longer loaded
	     (unless (anonymous-concept-p c)
	       (remhash (name c) *loaded-concept-names*))
	     ;; minimize it
	     (minimize-concept c)
	  )
    )
    ; Here there should be no more references to the bulk of the stuff we
    ; traversed, so it should be able to be GC'd. Note that we don't traverse
    ; disjunctions backwards (e.g. if concept A utilizes disjunction (OR B C),
    ; and we're evicting B, we don't get to A). This is probably OK, since if a
    ; concept is in a disjunction, it was either named, or the concept the
    ; disjunction was used by was defined in the same file. If it was named, we
    ; minimized it, so it no longer has references into the rest of the evicted
    ; set. If the disjunction-using concept was in the same file, we evicted it
    ; too.
  ;; mark the file as a whole as no longer loaded
  (remhash (namestring file) *loaded-resource-files*))

(defun require-concept (name)
  "Ensure that the named concept is completely defined according to the
   resource of the name symbol's package."
  (unless (gethash name *loaded-concept-names*)
    (let ((rv (gethash (symbol-package name) *resource-versions*)))
      (when (and rv (get-files-for-symbol rv))
        (let ((files (funcall (get-files-for-symbol rv) rv name)))
	  (dolist (file files)
	    (require-dsl-file file
	        :provenance-name (intern (package-name (pkg rv)) :ld))))))
    (setf (gethash name *loaded-concept-names*) t)
    ))

(defun require-resource-version (pkg-name &key (permanently t))
  "Ensure that all files from the resource version identified by the package
   name are loaded. If :permanently t (the default), don't ever evict any of
   the files so loaded."
  (let* ((pkg (find-package pkg-name))
         (rv (gethash pkg *resource-versions*))
	 (files (funcall (get-all-files rv) rv)))
    (when *load-verbose*
      (format *standard-output* "; requiring ~s files from ~s~%" (length files) rv))
      (let ((*load-verbose* nil))
	(dolist (f files)
	  (require-dsl-file f :permanently permanently)))))

(defun require-all-resource-files ()
  "Ensure that all files from all resource versions with files to load are
   loaded."
  (maphash
      (lambda (pkg rv)
        (dolist (f (funcall (get-all-files rv) rv))
	  (load-dsl-file f
	      :provenance-name (intern (package-name (pkg rv)) :ld))))
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
	      "social-contract"
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
  ( :version "3.2b" :base-dir (data-subdir "VerbNet;*")
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

