open Graph

let make_student (name : string) (wishes : choice list) : student =
    {name = name; wishes = wishes }

let make_sport (id : int) (string) (name: string) (capacity : int)=
    {
    id = id;
    name = name ;
    capacity = capacity;
    }

let make_choice (sport_id : int) (priority: int) : choice =
    {
    sport_id = sport_id;
    priority = priority;
    }

let make_student id name wishes : student = { id; name; wishes }
let make_sport id name nb_max_student : sport = { id; name; nb_max_student }
let make_choice sport_id priority : choice = { sport_id; priority }

(* IDs reservés *)
let source_id = 1
let sink_id   = 0

(* Encodage pour éviter collisions student/sport *)
let student_node (sid:int) = 10_000 + sid
let sport_node   (spid:int) = 1_000_000 + spid

let arc_lbl ?(cost=0) capacity : flow_arc_lbl =
  { capacity; flow = 0; cost }

let build_sport_solver_graph (students:student list) (sports:sport list)
  : flow_arc_lbl graph =
  let g0 = empty_graph |> new_node sink_id |> new_node source_id in

  (* Ajout des sommets students *)
  let g1 =
    List.fold_left
      (fun g s -> new_node g (student_node s.id))
      g0 students
  in

  (* Ajout des sommets sports *)
  let g2 =
    List.fold_left
      (fun g sp -> new_node g (sport_node sp.id))
      g1 sports
  in

  (* Source -> chaque student (capacité 1) *)
  let g3 =
    List.fold_left
      (fun g s ->
         new_arc g { src = source_id; tgt = student_node s.id; lbl = arc_lbl 1 })
      g2 students
  in

  (* Chaque sport -> puits (capacité nb_max_student) *)
  let g4 =
    List.fold_left
      (fun g sp ->
         new_arc g { src = sport_node sp.id; tgt = sink_id; lbl = arc_lbl sp.nb_max_student })
      g3 sports
  in

  (* Student -> sport si souhait, coût = priorité *)
  List.fold_left
    (fun g s ->
       List.fold_left
         (fun g ch ->
            new_arc g {
              src = student_node s.id;
              tgt = sport_node ch.sport_id;
              lbl = arc_lbl ~cost:ch.priority 1
            })
         g s.wishes)
    g4 student