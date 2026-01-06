open Graph

(* Label d’arc pour un graphe “de flot”.
   Ici Bellman-Ford n’utilise que cost (capacity/flow sont ignorés). *)
type flow_arc_lbl = { capacity:int; flow:int; cost:int }

(* helper de construction de label (optionnel) *)
let init_flow_arc_lbl cap flw cst = { capacity = cap; flow = flw; cost = cst }

(* bellmanford : calcule un plus court chemin de src vers dst en utilisant lbl.cost comme poids.
   Retour :
   - Some [a1; a2; ...] : chemin sous forme de liste d’arcs int (lbl = cost) de src -> dst
   - None : dst inatteignable depuis src
   Exceptions :
   - Graph_error si src/dst n’existent pas
   - Graph_error "Negative cycle" si un cycle négatif atteignable est détecté *)
let bellmanford (gr : flow_arc_lbl graph) (src : id) (dst : id) : int arc list option =
  (* Vérifie que src et dst existent *)
  if not (node_exists gr src) || not (node_exists gr dst) then
    raise (Graph_error "bellmanford: src/dst inconnu")
  (* Cas trivial : src = dst => chemin vide *)
  else if src = dst then
    Some []
  else
    (* Récupère tous les sommets (vs) + leur nombre (n) *)
    let (vs, n) = n_fold gr (fun (acc,k) v -> (v::acc, k+1)) ([],0) in

    (* dist[v] = None => +inf (inatteignable), Some d => distance depuis src
       pred[v] = Some arc => meilleur arc entrant vers v pour reconstruire le chemin *)
    let dist = Hashtbl.create (2*n+1) and pred = Hashtbl.create (2*n+1) in

    (* Init : tout à +inf, pas de prédécesseur *)
    List.iter (fun v -> Hashtbl.replace dist v None; Hashtbl.replace pred v None) vs;

    (* Init : distance de la source à 0 *)
    Hashtbl.replace dist src (Some 0);

    (* Relaxation d’un arc a : si dist[src] est connu et améliore dist[tgt],
       on met à jour dist[tgt] et pred[tgt]. *)
    let relax (a : flow_arc_lbl arc) =
      match Hashtbl.find_opt dist a.src with
      | Some (Some du) ->
          let nd = du + a.lbl.cost in
          (match Hashtbl.find_opt dist a.tgt with
           | Some (Some dv) when nd >= dv ->
               false (* pas d'amélioration *)
           | _ ->
               (* amélioration => on retient la nouvelle distance et l'arc prédécesseur *)
               Hashtbl.replace dist a.tgt (Some nd);
               Hashtbl.replace pred a.tgt (Some a);
               true)
      | _ ->
          false (* src inatteignable => impossible d’améliorer *)
    in

    (* Effectue jusqu’à |V|-1 passes de relaxation sur tous les arcs.
       Arrêt anticipé : si aucune relaxation n’a eu lieu, on peut s’arrêter. *)
    let rec passes k =
      if k = 0 then ()
      else
        let changed = ref false in
        e_iter gr (fun a -> if relax a then changed := true);
        if !changed then passes (k - 1)
    in
    passes (max 0 (n - 1));

    (* Détection cycle négatif atteignable :
       si après |V|-1 passes on peut encore améliorer une distance, il y a un cycle négatif. *)
    let neg = ref false in
    e_iter gr (fun a ->
      if not !neg then
        match Hashtbl.find_opt dist a.src, Hashtbl.find_opt dist a.tgt with
        | Some (Some du), Some (Some dv) when du + a.lbl.cost < dv -> neg := true
        | _ -> ()
    );
    if !neg then raise (Graph_error "Negative cycle");

    (* Si dst est inatteignable => None *)
    match Hashtbl.find_opt dist dst with
    | None | Some None -> None
    | Some (Some _) ->
        (* Reconstruit le chemin en remontant pred[] depuis dst jusqu’à src.
           On construit une liste d’arcs int où lbl = cost, dans l’ordre src -> dst. *)
        let rec build v acc =
          if v = src then Some acc
          else
            match Hashtbl.find_opt pred v with
            | Some (Some a) ->
                (* Comme on remonte, on préfixe l’arc ; acc est donc déjà dans le bon ordre final *)
                build a.src ({ src=a.src; tgt=a.tgt; lbl=a.lbl.cost } :: acc)
            | _ -> None (* incohérence : dst annoncé atteignable mais pas de prédécesseur *)
        in
        build dst []