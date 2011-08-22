;;;; HTML documentation generator with simple localization support.

(load #P"~/.clisprc.lisp")

(asdf:load-system :lml)
(use-package :lml)

(asdf:load-system :css-sexp)
(use-package :css-sexp)

(asdf:load-system :utils)
(use-package :utils)

;; Parse command line and find the language option
(defvar *lang* "en")

(loop for arg = *args* then (setf arg (cdr arg)) while arg do
  (if (string= (car arg) "--lang")
    (setf *lang* (cadr arg))))

;; Load the localization and the template
(load (string-concat "doc_" *lang* ".lisp"))
(load "doc-template.lisp")
