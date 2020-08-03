default: 
	$(MAKE) all

-include Makefile.ocaml

build::
	$(DUNE) build --only-packages tjr_kv @install
	$(DUNE) build bin/tjr_kv_test.exe bin/test.exe

update_generated_doc::
	cd src && (ocamldoc_pyexpander kv_store_with_lru.ml)
	cd src && (ocamldoc_pyexpander summary.t.ml > summary.ml)


run_tests:
#	$(DUNE) exec bin/test.exe
	OCAMLRUNPARAM=b $(DUNE) exec bin/tjr_kv_test.exe

# for auto-completion of Makefile target
clean::

