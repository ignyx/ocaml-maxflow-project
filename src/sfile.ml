open Sport

type path = string

(* Format of text files:
   % This is a comment

   % Sports classes with an id, max size, and name.
   c 1 20 Badminton
   c 2 15 Judo

   % Students with an id, and name
   s 1 Alice
   s 2 Bob

   % Wishes. Each student gets multiple wishes.
   % w <student id> <sport ID> <ranking>
   % Alice's first choice (Judo)
   w 1 2 1
   % Alice's second choice (Badminton)
   w 1 1 2
   % Bob's first choice (Badminton)
   w 2 1 1
   % Bob's second choice (Judo)
   w 2 2 2

*)

(* Reads a line with a sport's class. *)
let read_class db line =
  try Scanf.sscanf line "c %d %d %s@%%" (new_class db)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!"
      (Printexc.to_string e) line;
    failwith "from_file"

(* Reads a line with a student. *)
let read_student db line =
  try Scanf.sscanf line "s %d %s@%%" (new_student db)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!"
      (Printexc.to_string e) line;
    failwith "from_file"

(* Reads a line with a wish. *)
let read_wish db line =
  try Scanf.sscanf line "w %d %d %d" (new_wish db)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!"
      (Printexc.to_string e) line;
    failwith "from_file"

(* Reads a comment or fail. *)
let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line;
    failwith "from_file"

let from_sports_file path =
  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop graph =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let graph2 =
        (* Ignore empty lines *)
        if line = "" then graph
        (* The first character of a line determines its content : n or e. *)
          else
          match line.[0] with
          | 'c' -> read_class graph line
          | 's' -> read_student graph line
          | 'w' -> read_wish graph line
          (* It should be a comment, otherwise we complain. *)
          | _ -> read_comment graph line
      in
      loop graph2
    with End_of_file -> graph (* Done *)
  in

  let final_db = loop empty_db in

  close_in infile;
  final_db

let export_groups (p : path) (groups : sport_group list) =
  let join_names names =
    List.fold_left (fun acu name -> acu ^ "\n" ^ name) "" names
  in
  let string_of_group group =
    Printf.sprintf "# %s\n%s\n\n" group.sport (join_names group.students_list)
  in
  let group_strings = List.map string_of_group groups in
  let content = List.fold_left ( ^ ) "" group_strings in
  let ff = open_out p in
  Printf.fprintf ff "%s" content;
  close_out ff
