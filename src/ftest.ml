open Gfile
open Tools
open Graph
open Dfs
open Dijkstra
open Graph
    
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in
  let int_graph = gmap graph int_of_string in

  (* Apply changes *)
  let modified_graph = add_arc int_graph 0 5 3 in


  (* Rewrite the graph that has been read. *)
  let output_string_graph = gmap modified_graph string_of_int in
  let () = write_file outfile output_string_graph in

  (*Dfs TESTS*)
  let () = export "out.txt" graph in

  let shortest_path = dfs graph [] _source _sink in
  let display (path: 'a arc list option) = match path with
    | None -> Printf.printf "no path found"
    | Some arcs ->
        let rec aux (arcs: 'a arc list) = match arcs with
            | [] -> ()
            | h::t -> Printf.printf "%d--(%s)-->%d/" h.src h.lbl h.tgt; aux t
        in aux arcs
  in 
  let () = display shortest_path




  (*Djistra test *)
  let () = export outfile graph in
  let () =
    let min_path = dijkstra int_graph _source _sink in
    match min_path with
     | None -> Printf.printf "No path form Dijkstra"
     | Some path ->
            let rec print_arcs (arcs:'a arc list) = match arcs with
                |[]->()
                | a::b -> Printf.printf "%d--(%d)-->%d | " a.src a.lbl a.tgt; print_arcs b
               in
               print_arcs path
       in

  ()
