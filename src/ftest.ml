open Gfile
open Tools
open Graph
open Dfs
open Dijkstra
open Bellmanford
open Max_flow
open Max_flow_min_cost

(* Prints arc list to stdout *)
let rec print_path string_of_label = function
  | [] -> ()
  | arc :: tail ->
    Printf.printf "%d--(%s)-->%d/" arc.src (string_of_label arc.lbl) arc.tgt;
    print_path string_of_label tail

(* Prints arc list option to stdout *)
let print_path_opt string_of_label = function
  | None -> Printf.printf "None"
  | Some arcs -> print_path string_of_label arcs

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
  (* TODO rename the above *)

  (* Open file *)
  let graph = from_file infile in
  let int_graph = gmap graph int_of_string in

  let () = Printf.printf "Looking paths from %d to %d in the graph from %s\n" _source _sink infile in

  (* Dfs TESTS *)
  let dfs_path = dfs int_graph [] _source _sink in
  let () =
    Printf.printf "Path using DFS                   : ";
    print_path_opt string_of_int dfs_path;
    Printf.printf "\n"
  in

  (* Djistra test *)
  let dijkstra_path = dijkstra int_graph _source _sink in
  let () =
    Printf.printf "Shortest path using Dijkstra     : ";
    print_path_opt string_of_int dijkstra_path;
    Printf.printf "\n"
  in

  (* Bellman-Ford test *)
  let flow_graph = gmap int_graph (fun x -> { capacity = x; cost = x; flow = 1 }) in
  let bellmanford_path = bellmanford flow_graph _source _sink in
  let () =
    Printf.printf "Shortest path using Bellman-Ford : ";
    print_path_opt (fun lbl -> string_of_int lbl.cost) bellmanford_path;
    Printf.printf "\n"
  in

  (* Ford-Fulkerson *)
  let flow_graph = ford_fulkerson int_graph _source _sink in
  let labeled_graph =
    gmap flow_graph (fun lbl -> Printf.sprintf "%d/%d" lbl.flow lbl.capacity)
  in
  let () =
    export "out-ff.txt" labeled_graph;
    Printf.printf "Output Ford-Fulkerson graphviz to out-ff.txt\n"
  in

  (* Busacker-Gowen *)
  let input_graph =
    gmap int_graph (fun i -> { capacity = i; flow = 0; cost = i })
  in
  let flow_cost_graph = busacker_gowen input_graph _source _sink in
  let labeled_graph =
    gmap flow_cost_graph (fun lbl ->
        Printf.sprintf "%d/%d (%d)" lbl.flow lbl.capacity lbl.cost)
  in
  let () =
    export outfile labeled_graph;
    export "out-bg.txt" labeled_graph;
    Printf.printf "Output Busacker-Gowen graphviz to out-bg.txt and %s\n" outfile
  in
  ()
