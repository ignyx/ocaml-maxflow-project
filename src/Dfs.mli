open Graph

(* `dfs gr visited src tgt` performs a depth-first path search through graph from node src to node tgt,
   having already visited (or banned) the nodes in visited.
   Returns the traversed arcs, in order. *)
val dfs : 'a graph -> id list -> id -> id -> 'a arc list option
