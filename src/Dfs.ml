open Graph


(* Takes a list and a element and returns true if list contains e*)
let rec contains (l: 'a list) (e: 'a )=
    match l with
    | a::b -> if a = e then true else contains b e
    |[] -> false

let dfs (gr: 'a graph) (s: id) (e: id) : 'a arc list option =
    (* adds "if exist s and e"*)
    let visited = [] in
    let rec find_path (i: id): 'a arc list option =
        let neighbors_arcs = out_arcs gr i in
        (* browses every neighbor to find a path to 'e' *)
        let rec browse_neigh (neigh: 'a arc list) : 'a arc list option =
            match neigh with
                | [] -> None
                | next_arc::r ->
                begin
                    if(contains visited next_arc) then
                        (* already visited*)
                        None
                    else if(next_arc.tgt = e) then
                        (* target 'e' reached*)
                        Some [next_arc]
                    else
                    begin
                        (* finds a path from 'next_arc.tgt' to 'e'*)
                        let path_to_the_end = find_path next_arc.tgt in
                        match path_to_the_end with
                             | None -> browse_neigh r
                             | Some path -> Some (next_arc::path)
                    end
                end
        in browse_neigh neighbors_arcs
    in find_path s




