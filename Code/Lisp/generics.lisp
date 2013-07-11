;;;; generic function declarations (must be loaded before any other code using them, to avoid breaking some Lisps)

(in-package :dsl)

(defgeneric merge-concepts (dst src) (:documentation
  "Add the information in concept src to concept dst, destructively. Signal an
   error if the two conflict."))

(defgeneric listify (o) (:documentation
  "Turn an object into a pure nested list representation for printing."))

