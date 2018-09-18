SHELL:=/bin/bash
BASH_ENV=bash_env.sh
export BASH_ENV

# shouldn't shell builtin source bash_env anyway?
libname:=$(shell source bash_env.sh && echo $${libname})#
# $(info libname is $(libname)!)

# ----------------------------------------

build: 
	$$ocamlc -c $$mls
	$$ocamlopt -c $$mls
	mk_cma
	mk_cmxa

#	$(MAKE) install

install:
	mk_meta
	-remove
	install

uninstall:
	remove

remove: 
	remove

reinstall: 
	remove
	install

doc:
	echo Make sure to build first
	mk_doc
# 	cp *.html *.css ../docs

clean:
	clean
	rm -f *.html *.css


FORCE:
