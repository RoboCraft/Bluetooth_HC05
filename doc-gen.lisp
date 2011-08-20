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

;;; Some useful macros

;~ (defmacro tag (&body body)
  ;~ `(with-output-to-string (*html-output*)
    ;~ ,@body))
;~ 
(defmacro css (&body body)
  `(with-output-to-string (css-output)
    (cssexp:with-css-output (css-output)
      ,@body)))

(defmacro style (&rest args)
  `(with style ,@args))

;; Load the localization and the template
(load (string-concat "doc_" *lang* ".lisp"))
(load "doc-template.lisp")
