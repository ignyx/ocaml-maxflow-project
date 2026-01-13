open Graph
open Bellmanford

exception Sport_error of string

type sport_class = { id : int; capacity : int; name : string }
type student = { id : int; name : string }

type wish = {
  student_id : int;
  sport_id : int;
  priority : int; (* Higher preference is lower. Must be >= 0 *)
}

type sports_db = {
  classes : sport_class list;
  students : student list;
  wishes : wish list;
}

type sport_group = { sport : string; students_list : string list }

let empty_db = { classes = []; students = []; wishes = [] }

let class_exists db id =
  List.exists (fun (cl : sport_class) -> cl.id = id) db.classes

let new_class db id capacity name =
  if class_exists db id then
    raise
      (Sport_error ("class with id " ^ string_of_int id ^ " already exists."))
  else { db with classes = { id; name; capacity } :: db.classes }

let student_exists db id = List.exists (fun cl -> cl.id = id) db.students

let new_student db id name =
  if student_exists db id then
    raise
      (Sport_error ("student with id " ^ string_of_int id ^ " already exists."))
  else { db with students = { id; name } :: db.students }

let new_wish db student_id sport_id priority =
  match (student_exists db student_id, class_exists db sport_id) with
  | false, _ ->
    raise
      (Sport_error
         ("student with id " ^ string_of_int student_id ^ " doesn't exists."))
  | _, false ->
    raise
      (Sport_error
         ("class with id " ^ string_of_int sport_id ^ " doesn't exists."))
  | true, true ->
    { db with wishes = { student_id; sport_id; priority } :: db.wishes }

(* IDs reservés *)
let source_id = 0
let sink_id = 1

(* Encodage pour éviter collisions student/sport *)
let student_node (sid : int) = 10_000 + sid
let sport_node (spid : int) = 1_000_000 + spid
(* let arc_lbl ?(cost = 0) capacity : flow_arc_lbl = { capacity; flow = 0; cost } *)

let build_sport_solver_graph db =
  (* Add source and sink nodes *)
  let g0 = new_node (new_node empty_graph sink_id) source_id in

  (* Ajout des sommets students *)
  let g1 =
    List.fold_left (fun g s -> new_node g (student_node s.id)) g0 db.students
  in

  (* Ajout des sommets sports *)
  let g2 =
    List.fold_left
      (fun g (sp : sport_class) -> new_node g (sport_node sp.id))
      g1 db.classes
  in

  (* Source -> chaque student (capacité 1) *)
  let g3 =
    List.fold_left
      (fun g s ->
         new_arc g
           {
             src = source_id;
             tgt = student_node s.id;
             lbl = { capacity = 1; flow = 0; cost = 0 };
           })
      g2 db.students
  in

  (* Chaque sport -> puits (capacité nb_max_student) *)
  let g4 =
    List.fold_left
      (fun g (sp : sport_class) ->
         new_arc g
           {
             src = sport_node sp.id;
             tgt = sink_id;
             lbl = { capacity = sp.capacity; flow = 0; cost = 0 };
           })
      g3 db.classes
  in

  (* Student -> sport si souhait, coût = priorité *)
  List.fold_left
    (fun g w ->
       new_arc g
         {
           src = student_node w.student_id;
           tgt = sport_node w.sport_id;
           lbl = { capacity = 1; flow = 0; cost = w.priority };
         })
    g4 db.wishes

let flow_graph_to_group_lists db graph =
  let student_name_in_class sport_id student =
    if
      List.exists
        (fun arc -> arc.tgt = sport_node sport_id && arc.lbl.flow = 1)
        (out_arcs graph (student_node student.id))
    then Some student.name
    else None
  in
  let get_students_in_class sport_id =
    List.filter_map (student_name_in_class sport_id) db.students
  in
  List.map
    (fun (sport : sport_class) ->
       { sport = sport.name; students_list = get_students_in_class sport.id })
    db.classes

let node_id_to_name db = function
  | 0 -> "source"
  | 1 -> "puit"
  | sport_id when sport_id >= 1_000_000 ->
    (List.find
       (fun (sport : sport_class) -> sport.id = sport_id - 1_000_000)
       db.classes)
    .name
  | student_id when student_id >= 10_000 ->
    (List.find
       (fun (student : student) -> student.id = student_id - 10_000)
       db.students)
    .name
  | x -> string_of_int x
