open Graph

type flow_arc_lbl = { capacity : int; flow : int }

(* `ford_fulkerson gr src tgt` applies the Ford-Fulkerson algorithm to the
   capacity graph gr from node src to node tgt.*)
val ford_fulkerson : int graph -> id -> id -> flow_arc_lbl graph
