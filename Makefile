TMP_DOC_DIR:=/tmp/tjr_kv
scratch:=/tmp/l/github/scratch

default: all

-include Makefile.ocaml

build::
	$(DUNE) build --only-packages tjr_kv @install
	$(DUNE) build bin/tjr_kv_test.exe bin/test.exe

run_tests:
	$(DUNE) exec bin/test.exe
	$(DUNE) exec bin/tjr_kv_test.exe

# for auto-completion of Makefile target
clean::

