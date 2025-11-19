open Graph

let clone_nodes gr = n_fold gr new_node empty_graph

let gmap_entire_arc gr f =
  e_fold gr (fun acu edge -> new_arc acu (f edge)) (clone_nodes gr)

let gmap gr f = gmap_entire_arc gr (fun edge -> { edge with lbl = f edge.lbl })

let add_arc gr id1 id2 n =
  let add_if_target_edge edge =
    if edge.src = id1 && edge.tgt = id2 then { edge with lbl = edge.lbl + n }
    else edge
  in
  match find_arc gr id1 id2 with
  | None -> new_arc gr { src = id1; tgt = id2; lbl = n }
  | Some _edge -> gmap_entire_arc gr add_if_target_edge
