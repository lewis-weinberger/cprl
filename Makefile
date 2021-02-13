LISP ?= sbcl

build:
	$(LISP) --eval '(ql:quickload :cprl)' \
	        --eval '(asdf:make :cprl)' \
	        --eval '(quit)'

clean:
	rm cprl
