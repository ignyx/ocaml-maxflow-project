open Graph

let clone_nodes gr = n_fold gr new_node empty_graph

let gmap gr f =
  e_fold gr
    (fun acu edge -> new_arc acu { edge with lbl = f edge.lbl })
    (clone_nodes gr)

let add_arc gr id1 id2 n =
  let initial_value =
    match find_arc gr id1 id2 with None -> 0 | Some edge -> edge.lbl
  in
  new_arc gr { src = id1; tgt = id2; lbl = initial_value + n }
