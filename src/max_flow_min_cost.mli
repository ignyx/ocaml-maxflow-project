open Graph
open Bellmanford

(* `busacker_gowen gr src tgt` applies the Busacker-Gowen algorithm (using
   Bellman-Ford as a path-finding algorithm) to the capacity graph gr from node
   src to node tgt.
  Does NOT support return arcs (max 1 arc between each pair of nodes).
  TODO confirm line above.
  Costs must be positive.
  TODO confirm line above.
  *)
val busacker_gowen :
  flow_cost_arc_lbl graph -> id -> id -> flow_cost_arc_lbl graph
