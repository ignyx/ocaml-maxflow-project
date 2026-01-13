open Graph
open Bellmanford

(* `busacker_gowen gr src tgt` applies the Busacker-Gowen algorithm (using
   Bellman-Ford as a path-finding algorithm) to the capacity graph gr from node
   src to node tgt.
  Does NOT support return arcs (max 1 arc between each pair of nodes).
  Costs must be positive, or at least without any negative cycle.
  *)
val busacker_gowen :
  flow_cost_arc_lbl graph -> id -> id -> flow_cost_arc_lbl graph
