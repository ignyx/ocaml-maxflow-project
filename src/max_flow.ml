open Graph
open Tools
open Dfs

type flow_arc_lbl = { capacity : int; flow : int }

(* `get_min_flow_increase path` calculates the minimum possible flow increase along this `path`. *)
let get_max_flow_increase gap_path =
  let gaps = List.map (fun arc -> arc.lbl) gap_path in
  let initial = match gaps with [] -> 0 | x :: _ -> x in
  List.fold_left min initial gaps

(* Builds the flow graph, with a label containing both the capacity and flow, useful for display *)
let gap_to_flow_graph capacity_gr gap_gr =
  let mapp capacity_arc =
    (* the flow in the arc is equal to how much it can be reduced *)
    let flow =
      match find_arc gap_gr capacity_arc.tgt capacity_arc.src with
      | None -> 0
      | Some reducing_gap_arc -> reducing_gap_arc.lbl
    in
    {
      src = capacity_arc.src;
      tgt = capacity_arc.tgt;
      lbl = { capacity = capacity_arc.lbl; flow };
    }
  in
  gmap_entire_arc capacity_gr mapp

let ford_fulkerson capacity_gr src tgt =
  let rec apply_step gap_graph =
    (* filter out arcs with no possible flow increase *)
    let path = filtered_dfs (( < ) 0) gap_graph [] src tgt in
    match path with
    | None -> gap_graph
    | Some path ->
        let flow_increase = get_max_flow_increase path in
        let update_gaps graph1 arc =
          (* decrease valuation along arc *)
          let graph2 = add_arc graph1 arc.src arc.tgt (-flow_increase) in
          (* increase valuation along return arc *)
          add_arc graph2 arc.tgt arc.src flow_increase
        in
        let updated_capacity_graph =
          List.fold_left update_gaps gap_graph path
        in
        apply_step updated_capacity_graph
  in
  let final_gap_graph = apply_step capacity_gr in
  gap_to_flow_graph capacity_gr final_gap_graph
