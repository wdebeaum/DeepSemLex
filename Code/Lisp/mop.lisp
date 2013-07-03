(in-package :dsl)

;; import implementation-specific MOP stuff
(let ((mop-pkg
	#+abcl :mop
	#+allegro :hcl
	#+ccl :ccl
	#+clisp :clos
	#+cmu :pcl
	#+ecl :clos
        #+sbcl :sb-mop
	))
  (import (mapcar (lambda (f) (find-symbol f mop-pkg))
                  '("FINALIZE-INHERITANCE"
		    "SLOT-DEFINITION-NAME"
		    "CLASS-SLOTS"
		    ))))

