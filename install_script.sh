#!/bin/bash

opam install -y dune ocamlfind odoc
opam pin add -y -n tjr_lib_core https://github.com/tomjridge/tjr_lib.git
opam pin add -y -n tjr_lib https://github.com/tomjridge/tjr_lib.git
opam pin add -y -n tjr_fs_shared https://github.com/tomjridge/tjr_fs_shared.git
opam pin add -y -n tjr_monad https://github.com/tomjridge/tjr_monad.git
opam pin add -y -n tjr_profile https://github.com/tomjridge/tjr_profile.git
opam pin add -y -n isa_btree https://github.com/tomjridge/isa_btree.git
opam pin add -y -n tjr_btree https://github.com/tomjridge/tjr_btree.git#dev
opam pin add -y -n tjr_btree_examples https://github.com/tomjridge/tjr_btree.git#dev
opam pin add -y -n tjr_lru_cache https://github.com/tomjridge/tjr_lru_cache.git
opam pin add -y -n tjr_pcache https://github.com/tomjridge/tjr_pcache.git
opam pin add -y -n tjr_pcache_example https://github.com/tomjridge/tjr_pcache.git
opam install -y tjr_btree tjr_btree_examples tjr_lru_cache tjr_pcache

# NOTE this only installs dependencies at the moment
