open Bellmanford
open Graph

exception Sport_error of string

(* Stores the data prior conversion to graph *)
type sports_db

(* Empty database, without any classes, students or wishes *)
val empty_db : sports_db

(* `new_class id capacity name` adds a class to which at most `capacity` students
   will be assigned.
   raise @Sport_error if id already exists *)
val new_class : sports_db -> int -> int -> string -> sports_db

(* `new_student id name` adds a student.
   raise @Sport_error if id already exists *)
val new_student : sports_db -> int -> string -> sports_db

(* `new_wish student_id sport_id priority` adds wish for a student.
   Priority 1 means this class is the student's top choice, while 2 means second choice.
   raise @Sport_error if student or class doesn't exist. *)
val new_wish : sports_db -> int -> int -> int -> sports_db

(* Converts the sport_db to a Flow Graph to solve *)
val build_sport_solver_graph : sports_db -> flow_cost_arc_lbl graph

type sport_group = { sport : string; students_list : string list }

(* After solving the Flow Graph, retrieve the groups *)
val flow_graph_to_group_lists :
  sports_db -> flow_cost_arc_lbl graph -> sport_group list
