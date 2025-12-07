open Graph

let rec dfs (gr : 'a graph) (visited : id list) (src : id) (tgt : id) :
    'a arc list option =
  if src = tgt then Some []
  else
    let path_from_neighbor arc =
      (* omit already visited nodes *)
      if List.mem arc.tgt visited then None
      else
        (* try to find a path from each neighbor to tgt *)
        match dfs gr (src :: visited) arc.tgt tgt with
        | None -> None
        (* when successful, prepend current arc *)
        | Some path -> Some (arc :: path)
    in
    List.find_map path_from_neighbor (out_arcs gr src)
