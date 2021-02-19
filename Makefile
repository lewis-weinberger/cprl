LISP ?= sbcl

SRC = cprl.asd \
      src/bazaar.lisp \
      src/blt.lisp \
      src/config.lisp \
      src/entity.lisp \
      src/jobs.lisp \
      src/location.lisp \
      src/macros.lisp \
      src/main.lisp \
      src/packages.lisp \
      src/player.lisp \
      src/ui.lisp

bin/cprl: $(SRC)
	$(LISP) --eval '(ql:quickload :cprl)' \
	        --eval '(asdf:make :cprl)' \
	        --eval '(quit)'

clean:
	rm -rf bin/

.PHONY: clean
