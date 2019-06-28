
# tjr_kv, a key-value store for OCaml

This is a key value store written in OCaml. It uses the B-tree code
from tjr_btree, but adds an in-memory LRU cache (or just "cache") in
front, and a persistent cache (pcache) between the cache and the
B-tree. The pcache and the B-tree are synchronous: updates are written
directly to disk.


![](https://docs.google.com/drawings/d/e/2PACX-1vQc8669_M4bqjDZNCC9KoUYSx7ZNOWbGMtUOiZJFfgoLGc3jFZeamg6_BydB_ZzhZ4CViHV1q-t0QZh/pub?w=960&amp;h=720)

The LRU should provide (but doesn't currently!) an interface similar to:

![2019-06-28.142813](/tmp/l/github/an_imp/d_tjr_kv/README.assets/2019-06-28.142813.png)

## Quick links

* For ocamldoc, see <https://tomjridge.github.io/tjr_kv/>
* The main interfaces are in <https://tomjridge.github.io/tjr_kv/tjr_kv/Tjr_kv/Kv_intf/index.html>

## Glossary

* Block: the "natural" unit of storage for a disk, consisting of a certain number of bytes; typically 4096 bytes in length, in which case block n starts at index n*4096
* B-tree: an on-disk balanced search tree; provides the backend data store; provides quick access to values indexed by keys
* Detachable map or dmap: this is an on-disk log starting from a known root block; it supports a "detach" operation, which collects the tail of the log  (and then typically inserts the entries into the B-tree). See <https://github.com/tomjridge/tjr_pcache>. Also known as a "persistent cache" or "pcache".
* LRU: short for "least-recently-used cache", a popular caching strategy; see <https://en.wikipedia.org/wiki/Cache_replacement_policies#Least_recently_used_(LRU)>
* Message queue: <https://en.wikipedia.org/wiki/Message_queue>; used for passing messages between components of a system. For us, the main message queues are q_lru_dmap and q_dmap_bt
* Persistent cache, or pcache: see Dmap.
* Roll-up map, or RUM: a detachable map linked to a B-tree. At intervals, the tail of the dmap is detached and inserted into the B-tree.
* Root manager: for on-disk entities, the root manager tracks the root block from which the entity can be reconstructed. For example, each B-tree has a root block, which typically contains pointers to further blocks. The dmap is another example of an on-disk entity with a root block.

## Dependencies

| Description   | Comment                        |
| ------------- | ------------------------------ |
| tjr_btree     | For the persistent on-disk map |
| tjr_pcache    | For the persistent cache       |
| tjr_lru_cache | For the LRU in-memory cache    |
| tjr_mem_queue | For inter-thread communication |

