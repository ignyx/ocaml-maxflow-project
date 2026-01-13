open Sfile
open Gfile
open Sport
open Tools
open Bellmanford
open Max_flow_min_cost

let () =
  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 3 then (
    Printf.printf "\n ✻  Usage: %s infile outfile\n\n%s%!" Sys.argv.(0)
      ("    🟄  infile  : input file containing the student choices.\n"
     ^ "    🟄  outfile : output file in which the result attributions should \
        be written.\n\n");
    exit 0);

  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  let infile = Sys.argv.(1) and outfile = Sys.argv.(2) in

  (* Open file *)
  let db = from_sports_file infile in

  (* Build flow graph *)
  let initial_graph = build_sport_solver_graph db in

  (* Solve for max flow min cost *)
  let solved_graph = busacker_gowen initial_graph 0 1 in
  let initial_labeled_graph =
    gmap solved_graph (fun lbl ->
        Printf.sprintf "%d/%d (%d)" lbl.flow lbl.capacity lbl.cost)
  in
  let () =
    export outfile initial_labeled_graph
    (* Printf.printf "Output Busacker-Gowen graphviz to out-bg.txt and %s\n" outfile *)
  in
  ()
