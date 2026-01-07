open Graph

type flow_cost_arc_lbl = { capacity : int; flow : int; cost : int }

val bellmanford : flow_cost_arc_lbl graph -> id -> id -> flow_cost_arc_lbl arc list option
