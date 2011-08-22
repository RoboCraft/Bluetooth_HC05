LISP = clisp
LISP_OPTS =
DOC_GEN_SCRIPT = doc-gen.lisp
GEN_CMD = $(LISP) $(LISP_OPTS) $(DOC_GEN_SCRIPT)

all: doc-ru #doc-en

doc-ru: doc_ru.lisp
	$(GEN_CMD) --lang ru > doc_ru.html

doc-en: doc_en.lisp
	$(GEN_CMD) --lang en > doc_en.html
