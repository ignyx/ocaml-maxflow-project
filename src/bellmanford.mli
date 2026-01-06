open Graph

type flow_arc_lbl = { capacity:int; flow:int; cost:int }

val bellmanford : flow_arc_lbl graph -> id -> id -> int arc list option

val init_flow_arc_lbl : int -> int -> int -> flow_arc_lbl