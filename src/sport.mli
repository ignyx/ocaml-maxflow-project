open Graph

type choice = {
    sport_id : int;
    priority : int;
}

type student = {
    id : int;
    name : string;
    wishes : choice list;
}

type sport = {
    id : int;
    name : string;
    capacity : int;
    nb_max_student : int;
}

type sport_group = {
    sport : sport;
    students_list : student_list
}

(* builders *)
val make_student : string -> choice list -> student
val make_sport : int -> string -> int -> make
val make_choice : int -> int -> choice

val build_sport_solver_graph : student list -> sport list -> flow_arc_label graph
val make_groups : student list -> sport list -> sport_group list

