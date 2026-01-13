open Graph
open Stdlib


let infinity = max_int

(* Init only: distances + parent arcs + visited *)
let init_dijkstra (gr : int graph) (src : id) =
  let nodes = n_fold gr (fun acc v -> v :: acc) [] in
  let dist    = Hashtbl.create (2 * List.length nodes + 1) in
  let parent  = Hashtbl.create (2 * List.length nodes + 1) in  (* id -> int arc *)
  let visited = Hashtbl.create (2 * List.length nodes + 1) in
  List.iter (fun v ->
      Hashtbl.replace dist v infinity;
      Hashtbl.replace visited v false
    ) nodes;
  Hashtbl.replace dist src 0;
  (nodes, dist, parent, visited)

let dijkstra (gr : int graph) (src : id) (tgt : id) : int arc list option =
  if not (node_exists gr src) then raise (Graph_error "[DIJKSTRA] unknown source");
  if not (node_exists gr tgt) then raise (Graph_error "[DIJKSTRA] unknown target");
  if src = tgt then Some [] else

    let (nodes, dist, parent, visited) = init_dijkstra gr src in

    (*Extracts min form priority queue*)
    let extract_min () =
      let min = ref None in
      List.iter (fun v ->
          if not (Hashtbl.find visited v) then
            let dv = Hashtbl.find dist v in
            match !min with
            | None -> min := Some (v, dv)
            | Some (_, dmin) -> if dv < dmin then min := Some (v, dv)
        ) nodes;
      match !min with None -> None | Some (v, _) -> Some v
    in

    (*Main loop for Dijkstra*)
    let rec aux () =
      match extract_min () with
      | None -> ()
      | Some u ->
        (*Update visited*)
        Hashtbl.replace visited u true;
        if u = tgt then () else
          let du = Hashtbl.find dist u in
          if du <> infinity then
            (*Browses next nodes with arcs*)
            out_arcs gr u
            |> List.iter (fun a ->
                let v = a.tgt in
                if not (Hashtbl.find visited v) then
                  let temp = du + a.lbl in
                  let dv = Hashtbl.find dist v in
                  if temp < dv then (
                    (*Updates of the distance and the parent*)
                    Hashtbl.replace dist v temp;
                    Hashtbl.replace parent v a   (* store the parent ARC directly *)
                  )
              );
          aux ()
    in
    aux ();

    (* Finds the path form the parents HashMap*)
    if Hashtbl.find dist tgt = infinity then
      None
    else
      let rec build_path acc v : int arc list=
        if v = src then acc
        else
          match Hashtbl.find_opt parent v with
          | None -> raise (Graph_error "[DIJKSTRA] Parents issue")
          | Some a -> build_path (a :: acc) a.src
      in
      Some(build_path [] tgt)
