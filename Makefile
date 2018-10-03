build:
	dune build @install
	dune build bin/tjr_kv_test.exe

install:
	dune install

uninstall:
	dune uninstall

run_tests:
	dune exec bin/tjr_kv_test.exe

doc:
	dune build @doc

view_doc:
	google-chrome  _build/default/_doc/_html/index.html

clean:
	dune clean
