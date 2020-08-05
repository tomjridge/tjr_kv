(lang dune 2.0)
(name tjr_kv)

;; for opam
(authors              "Tom Ridge <tom.j.ridge@googlemail.com>")
(maintainers          "Tom Ridge <tom.j.ridge@googlemail.com>")
(source               (github tomjridge/tjr_kv))
(homepage             https://github.com/tomjridge/tjr_kv)
(documentation        https://tomjridge.github.io/ocamldocs/)
(generate_opam_files  true)


(package 
 (name tjr_kv)
 (synopsis "A Key-Value store for OCaml (part of ImpFS)")
 (depends 
  tjr_btree
  tjr_btree_examples
  tjr_lru_cache
  tjr_mem_queue
  tjr_pcache 
  tjr_pcache_example
))

