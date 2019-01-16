DUNE:=opam exec dune

build:
	$(DUNE) build @install
	$(DUNE) build bin/tjr_kv_test.exe bin/test.exe

install:
	$(DUNE) install

uninstall:
	$(DUNE) uninstall

run_tests:
	$(DUNE) exec bin/test.exe
	$(DUNE) exec bin/tjr_kv_test.exe

doc: FORCE
	$(DUNE) build @doc
	rm -rf /tmp/tjr_kv
	cp -R _build/default/_doc/_html/ /tmp/tjr_kv  # so we don't lose it on clean

view_doc:
	google-chrome  _build/default/_doc/_html/index.html

clean:
	$(DUNE) clean


FORCE:
