# tjr_kv, a key-value store for OCaml

This is a key value store written in OCaml. It uses the B-tree code
from tjr_btree, but adds an in-memory LRU cache (or just "cache") in
front, and a persistent cache (pcache) between the LRU cache and the
B-tree. 


![](https://docs.google.com/drawings/d/e/2PACX-1vTIXhyNa7dovQYXuJXBMwPQZU99-x_tRdTIH3SkMUDyPwbL31zExWXauT2hO-eRIUcnGP3RVHiSHrjt/pub?w=557&h=428)



## Demo

This is a demo (c. August 2019) of a version of the system with a B-tree placeholder. The demo runs for 2s. At the end, 800k writes have occurred at the LRU interface. 500k of these have been processed via the persistent cache and made it to the B-tree.


<a href="https://imgur.com/fHyug2t"><img src="https://i.imgur.com/fHyug2t.gif" title="source: imgur.com" /></a>



## Building

The easiest way to build is to look at the Dockerfile and copy the relevant RUN lines (without the RUN prefix). To build with docker just run `docker build .`



## Quick links

* For OCamldoc, Travis CI builds etc, see <https://tomjridge.github.io/ocamldocs/>



## Glossary

* Block: the "natural" unit of storage for a disk, consisting of a certain number of bytes; typically 4096 bytes in length, in which case block n starts at index n*4096
* B-tree: an on-disk balanced search tree; provides the backend data store; provides quick access to values indexed by keys
* LRU: short for "least-recently-used cache", a popular caching strategy; see <https://en.wikipedia.org/wiki/Cache_replacement_policies#Least_recently_used_(LRU)>
* Message queue: <https://en.wikipedia.org/wiki/Message_queue>; used for passing messages between components of a system. For us, the main message queues are q_lru_pc and q_pc_bt
* Persistent cache, or pcache: this is an on-disk log starting from a known root block; it supports a "detach" operation, which collects the tail of the log  (and then typically inserts the entries into the B-tree). See <https://github.com/tomjridge/tjr_pcache>. 
* Root manager: for on-disk entities, the root manager tracks the root block from which the entity can be reconstructed. For example, each B-tree has a root block, which typically contains pointers to further blocks. The dmap is another example of an on-disk entity with a root block.



## OCaml naming conventions

We use the following type variables/ fixed types:

* k for keys
* v for values
* r for pointers (integers); typically the same as block identifiers
* blk_id for block identifiers (integers)
* t for the generic monad phantom type



## OCaml code style

* We have tried to use records for the interfaces (rather than, say, signatures and functors) where possible. This was to enable switching of implementations at runtime. Nowadays this is perhaps better achieved with first-class modules.

* When listing type parameters, we try to stick to the following order: k, v, r/blk_id, blk, node, leaf, leaf_stream, t

  

## Dependencies

| Description   | Link                                       | Comment                        |
| ------------- | ------------------------------------------ | ------------------------------ |
| tjr_btree     | https://github.com/tomjridge/tjr_btree     | For the persistent on-disk map |
| tjr_pcache    | https://github.com/tomjridge/tjr_pcache    | For the persistent cache       |
| tjr_lru_cache | https://github.com/tomjridge/tjr_lru_cache | For the LRU in-memory cache    |
| tjr_mem_queue | https://github.com/tomjridge/tjr_mem_queue | For inter-thread communication |

