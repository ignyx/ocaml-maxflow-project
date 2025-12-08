open Gfile
open Tools
open Graph
open Dfs
open Max_flow

let () =
  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then (
    Printf.printf "\n ✻  Usage: %s infile source sink outfile\n\n%s%!"
      Sys.argv.(0)
      ("    🟄  infile  : input file containing a graph\n"
     ^ "    🟄  source  : identifier of the source vertex (used by the \
        ford-fulkerson algorithm)\n"
     ^ "    🟄  sink    : identifier of the sink vertex (ditto)\n"
     ^ "    🟄  outfile : output file in which the result should be written.\n\n"
      );
    exit 0);

  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3) in

  (* Open file *)
  let graph = from_file infile in
  let int_graph = gmap graph int_of_string in

  (* Apply changes *)
  let modified_graph = add_arc int_graph 0 5 3 in

  (* Rewrite the graph that has been read. *)
  let output_string_graph = gmap modified_graph string_of_int in
  let () = write_file outfile output_string_graph in

  let () = export "out.txt" graph in

  (*Dfs TESTS*)
  let shortest_path = dfs graph [] _source _sink in
  let display (path : 'a arc list option) =
    match path with
    | None -> Printf.printf "no path found\n"
    | Some arcs ->
        let rec aux (arcs : 'a arc list) =
          match arcs with
          | [] -> ()
          | h :: t ->
              Printf.printf "%d--(%s)-->%d/" h.src h.lbl h.tgt;
              aux t
        in
        aux arcs
  in
  let () =
    display shortest_path;
    Printf.printf "\n"
  in

  (* Ford-Fulkerson *)
  let flow_graph = ford_fulkerson int_graph _source _sink in
  let labeled_graph =
    gmap flow_graph (fun lbl -> Printf.sprintf "%d/%d" lbl.flow lbl.capacity)
  in
  let () =
    export "out-ff.txt" labeled_graph;
    Printf.printf "Output Ford-Fulkerson graphviz to out-ff.txt"
  in
  ()
