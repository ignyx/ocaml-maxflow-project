open Graph

type flow_cost_arc_lbl = { capacity : int; flow : int; cost : int }
(* TODO use above type in PCC algo, and rebuild at end once (instead of every time) *)

(* `get_min_flow_increase path` calculates the minimum possible flow increase along this `path`. *)
(* TODO refactor with sibling function ? *)
let get_max_flow_increase gap_path =
  let gaps = List.map (fun arc -> arc.lbl) gap_path in
  let initial = match gaps with [] -> 0 | x :: _ -> x in
  List.fold_left min initial gaps

(* adds flow to arc, or return arc.
   @raises Graph_error if nor the forward nor return arc exist *)
(* TODO refactor using a tuple for readiblity and add a comment about assuming the compiler won't always evaluate the second call *)
let add_arc_flow gr src tgt n =
  match find_arc gr src tgt with
  | Some forward_arc ->
      new_arc gr
        {
          forward_arc with
          lbl = { forward_arc.lbl with flow = forward_arc.lbl.flow + n };
        }
  (* can't find forward arc, look for return arc *)
  | None -> (
      match find_arc gr tgt src with
      | None ->
          raise
            (Graph_error
               ("Couldn't find an arc between " ^ string_of_int src ^ " and "
              ^ string_of_int tgt))
      | Some return_arc ->
          new_arc gr
            {
              return_arc with
              lbl = { return_arc.lbl with flow = return_arc.lbl.flow + n };
            })

(* TODO possibly can remove the helper func *)
let busacker_gowen input_graph _src _tgt =
  let rec apply_step flow_gr =
    (* TODO use actual path-finding algo *)
    let path = None in
    match path with
    (* If no path is found, we're done ! *)
    | None -> flow_gr
    (* Otherwise, update flows accordingly and repeat *)
    | Some path ->
        let flow_increase = get_max_flow_increase path in
        let add_flow acu gap_arc =
          add_arc_flow acu gap_arc.src gap_arc.tgt flow_increase
        in
        let updated_flow_gr = List.fold_left add_flow flow_gr path in
        apply_step updated_flow_gr
  in
  apply_step input_graph
