let noms = [|
  "Tour Eiffel";         (* 0  *)
  "Arc de Triomphe";     (* 1  *)
  "Champs-Élysées";      (* 2  *)
  "Place de la Concorde";(* 3  *)
  "Musée d'Orsay";       (* 4  *)
  "Louvre";              (* 5  *)
  "Palais-Royal";        (* 6  *)
  "Sacré-Cœur";          (* 7  *)
  "Notre-Dame";          (* 8  *)
  "Panthéon";            (* 9  *)
  "Jardins du Luxembourg";(* 10 *)
  "Place de la Bastille";(* 11 *)
  "Trocadéro";           (* 12 *)
  "Champ-de-Mars";       (* 13 *)
|]

let coords = [|
  (48.8584, 2.2945);  (* 0  Tour Eiffel *)
  (48.8738, 2.2950);  (* 1  Arc de Triomphe *)
  (48.8698, 2.3078);  (* 2  Champs-Élysées *)
  (48.8656, 2.3212);  (* 3  Place de la Concorde *)
  (48.8600, 2.3266);  (* 4  Musée d'Orsay *)
  (48.8606, 2.3376);  (* 5  Louvre *)
  (48.8638, 2.3370);  (* 6  Palais-Royal *)
  (48.8867, 2.3431);  (* 7  Sacré-Cœur *)
  (48.8530, 2.3499);  (* 8  Notre-Dame *)
  (48.8462, 2.3460);  (* 9  Panthéon *)
  (48.8462, 2.3372);  (* 10 Jardins du Luxembourg *)
  (48.8533, 2.3692);  (* 11 Place de la Bastille *)
  (48.8625, 2.2893);  (* 12 Trocadéro *)
  (48.8556, 2.2986);  (* 13 Champ-de-Mars *)
|]

let voisins = [|
  [|(4, 2.2); (13, 0.4); (12, 0.7)|];           (* 0  Tour Eiffel *)
  [|(2, 1.1)|];                                   (* 1  Arc de Triomphe *)
  [|(1, 1.1); (3, 1.0)|];                        (* 2  Champs-Élysées *)
  [|(2, 1.0); (4, 0.7); (5, 1.2)|];             (* 3  Place de la Concorde *)
  [|(0, 2.2); (3, 0.7); (5, 1.4); (10, 1.2)|];  (* 4  Musée d'Orsay *)
  [|(4, 1.4); (3, 1.2); (6, 0.2)|];             (* 5  Louvre *)
  [|(5, 0.2); (7, 3.1)|];                        (* 6  Palais-Royal *)
  [|(6, 3.1)|];                                   (* 7  Sacré-Cœur *)
  [|(9, 1.2); (11, 2.1)|];                       (* 8  Notre-Dame *)
  [|(8, 1.2); (10, 0.6)|];                       (* 9  Panthéon *)
  [|(9, 0.6); (4, 1.2)|];                        (* 10 Jardins du Luxembourg *)
  [|(8, 2.1)|];                                   (* 11 Place de la Bastille *)
  [|(0, 0.7)|];                                   (* 12 Trocadéro *)
  [|(0, 0.4)|];                                   (* 13 Champ-de-Mars *)
|]

let n = Array.length noms

let nom_to_num (nom : string) (noms : string array) : int  = 
  let rec search i =
    if i >= Array.length noms then
      failwith "nom pas dans la liste"
    else if noms.(i) = nom then
      i
    else
      search (i + 1)
  in
  search 0



(* Crée les structures pour stocker :
   - distances[i] = distance minimale connue pour atteindre le nœud i
   - parents[i] = le nœud d'où on vient pour atteindre i
   - visite[i] = true si on a déjà traité le nœud i *)
let dijkstra_init (depart : int) (n : int) : float array * int array * bool array =
  let distances = Array.make n Float.infinity in
  let parents = Array.make n (-1) in
  let visite = Array.make n false in
  distances.(depart) <- 0.;  (* La distance pour atteindre le départ est 0 *)
  (distances, parents, visite)


(* Trouve le nœud non visité avec la plus petite distance *) 
let trouver_min_non_visite (distances : float array) (visite : bool array) : int option =
  let min_dist = ref Float.infinity in
  let min_idx = ref (-1) in
  for i = 0 to Array.length distances - 1 do
    if not visite.(i) && distances.(i) < !min_dist then begin
      min_dist := distances.(i);
      min_idx := i
    end
  done;
  if !min_idx = -1 then None else Some !min_idx


(*compare le nouvelle distance (issue du parent optimal) avec la distance initiale, et met à jour le chemin si besoin *)
let comparaison (noeud : int) (distances : float array) (parents : int array) (visite : bool array) (voisins : (int * float) array array) : unit =
  let tab_noeud_voisins = voisins.(noeud) in
  Array.iter (fun (voisin, val_arete) ->
    if not visite.(voisin) then begin
      let nouvelle_distance = distances.(noeud) +. val_arete in
      if nouvelle_distance < distances.(voisin) then begin
        distances.(voisin) <- nouvelle_distance;
        parents.(voisin) <- noeud
      end
    end
  ) tab_noeud_voisins


(* algo final djikstra
let dijkstra (depart : int) (arrivee : int) (n : int) (voisins : (int * float) array array) : (int list * float) option =
  let (distances, parents, visite) = dijkstra_init depart n in
  
  for _ = 0 to n - 1 do
    match trouver_min_non_visite distances visite with
    | None -> ()  (* Tous les nœuds accessibles ont été traités *)
    | Some noeud ->
        visite.(noeud) <- true;
        comparaison noeud distances parents visite voisins
  done;
  
  if distances.(arrivee) = Float.infinity then
    None
  else
    (* Reconstruit le chemin en remontant les parents avec une liste acc *)
    let rec construire_chemin noeud acc =
      if noeud = -1 then acc
      else construire_chemin parents.(noeud) (noeud :: acc)
    in
    let chemin = construire_chemin arrivee [] in
    Some (chemin, distances.(arrivee))




let approx a b = abs_float (a -. b) < 0.01

let test ()=
  let (chemin, dist) = chemin_force_brute "Trocadéro" "Tour Eiffel" in
    assert (chemin = ["Trocadéro"; "Tour Eiffel"]);
    assert (dist = 0.7);
  
  let (chemin, dist) = chemin_force_brute "Panthéon" "Musée d'Orsay" in
    assert (approx dist 1.8);
    assert(chemin = ["Panthéon"; "Jardins du Luxembourg"; "Musée d'Orsay"]);

  print_string "Bravo :)"
