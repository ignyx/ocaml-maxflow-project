open Graph

let rec filtered_dfs (filter : 'a -> bool) (gr : 'a graph) (visited : id list)
    (src : id) (tgt : id) : 'a arc list option =
  if src = tgt then Some []
  else
    let path_from_neighbor arc =
      (* omit already visited nodes *)
      if List.mem arc.tgt visited || not (filter arc.lbl) then None
      else
        (* try to find a path from each neighbor to tgt *)
        match filtered_dfs filter gr (src :: visited) arc.tgt tgt with
        | None -> None
        (* when successful, prepend current arc *)
        | Some path -> Some (arc :: path)
    in
    List.find_map path_from_neighbor (out_arcs gr src)

(* We can't omit the last parameters otherise _ is weak *)
let dfs gr visited src tgt = filtered_dfs (fun _ -> true) gr visited src tgt
