open Graph
open Tools
open Bellmanford

(* type flow_cost_arc_lbl = { capacity : int; flow : int; cost : int } *)
(* TODO use above type in PCC algo, and rebuild at end once (instead of every time) *)

(* `get_min_flow_increase path` calculates the minimum possible flow increase along this `path`. *)
let get_max_flow_increase gap_path =
  let gaps = List.map (fun arc -> arc.lbl.flow) gap_path in
  let initial = match gaps with [] -> 0 | x :: _ -> x in
  List.fold_left min initial gaps

(* Perform flow updates on forward and return arc.
   Must be done together so we have the capacity and cost info to initialize the label.
   We assume the forward arc always exists.
   @raises Graph_error if no forward arc exists *)
let add_arc_flow gr src tgt n =
  (* Get forward arc *)
  let forward_arc =
    match find_arc gr src tgt with
    | Some forward_arc -> forward_arc
    | None ->
        raise
          (Graph_error
             ("Couldn't find a forward arc between " ^ string_of_int src
            ^ " and " ^ string_of_int tgt))
  in
  (* Update forward arc with increase flow *)
  let gr1 =
    new_arc gr
      {
        forward_arc with
        lbl = { forward_arc.lbl with flow = forward_arc.lbl.flow + n };
      }
  (* Get return arc flow *)
  and initial_return_arc_flow =
    match find_arc gr tgt src with
    | Some return_arc -> return_arc.lbl.flow
    | None -> 0
  in
  (* Update return arc with decreased flow *)
  new_arc gr1
    {
      src = tgt;
      tgt = src;
      lbl =
        {
          forward_arc.lbl with
          flow = initial_return_arc_flow - n;
          cost = -forward_arc.lbl.cost;
        };
    }

(* Builds the flow graph, with a label containing both the capacity, flow and cost, useful for display. *)
let gap_to_flow_graph capacity_gr gap_gr =
  let join flow_gr_acu capacity_arc =
    (* flow along the forward arc, if any *)
    let flow =
      match find_arc gap_gr capacity_arc.src capacity_arc.tgt with
      | None -> 0
      | Some arc -> capacity_arc.lbl.capacity - arc.lbl.flow
    in
    new_arc flow_gr_acu
      { capacity_arc with lbl = { capacity_arc.lbl with flow } }
  in
  e_fold capacity_gr join (clone_nodes capacity_gr)

let busacker_gowen input_graph src tgt =
  (* the _flow_ will correspond to the possible flow increase.
     Existing arcs will have a positive cost, return arcs will have a negative cost.
     A flow of 0 in the gap graph means the arc isn't there. *)
  let initial_flow_gr =
    gmap input_graph (fun edge -> { edge with flow = edge.capacity })
  in
  let rec apply_step flow_gr =
    let path = bellmanford flow_gr src tgt in
    match path with
    (* If no path is found, we're done ! *)
    | None | Some [] -> flow_gr
    (* Otherwise, update flows accordingly and repeat *)
    | Some path ->
        let flow_increase = get_max_flow_increase path in
        let add_flow acu gap_arc =
          (* decrease valuation along forward arc *)
          let graph1 =
            add_arc_flow acu gap_arc.src gap_arc.tgt (-flow_increase)
          in
          (* increase valuation along return arc *)
          graph1
          (* add_arc_flow graph1 gap_arc.tgt gap_arc.src (-flow_increase) *)
        in
        let updated_flow_gr = List.fold_left add_flow flow_gr path in
        apply_step updated_flow_gr
  in
  let final_gap_graph = apply_step initial_flow_gr in
  gap_to_flow_graph input_graph final_gap_graph
