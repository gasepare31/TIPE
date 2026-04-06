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

(* voisins.(i) = tableau de (indice_voisin, distance) *)
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

(* renvoie l'indice du nom *)
let nom_to_num (nom : string) (noms : string array) : int  = 
  match Array.find_index (fun x -> x = nom) noms with (* find_index revoie le rang du premier element qui satisfait la fonction, ici x -> x = nom *)
  | Some (x) -> x
  | None -> failwith "nom pas dans la liste"


(* renvoie l'ordre des voisins les plus proche de la droite (coefficient) *)
let proche (point : int) (coefficient: float) : int array = 
  let list_voisins = voisins.(point) in
  let n_voisins = Array.length (list_voisins) in 

  let tab = Array.make n_voisins (0, 0.) in 
  let x_pt, y_pt = coords.(point) in

  for i = 0 to n_voisins-1 do 
    let idx, _ = list_voisins.(i) in 
    let x, y = coords.(idx) in 
    if (x-.x_pt) <> 0. then 
      let coef = (y-.y_pt)/.(x-.x_pt) in 
      tab.(i) <- (idx, Float.abs (coefficient-.coef))
  done;
  
  Array.sort (fun (_, x) (_, y) -> Float.compare x y) tab; 

  Array.map (fun (x,_) -> x) tab


let chemin_direction (depart : string) (arrivee : string) : (int array) * float =
  let num_depart = nom_to_num depart noms in
  let num_arrivee = nom_to_num arrivee noms in

  let chemin = (Some (Array.make n (-1))) in
  let distance = ref 0. in
  let indice = ref 1 in

  let meilleur_chemin = ref [||] in
  let meilleur_distance = ref Float.infinity in

  let pt_actuel = ref num_depart in
  let visite = ref (Array.make n false) in 

  let x_arrivee, y_arrivee = coords.(num_arrivee) in 

  let autre_indice = ref 0 in

  let rec fonction_bla_bla (pt_actuel : int) (visites : bool array) : unit =
    if visites.(pt_actuel) then ()
    else begin
    visites.(pt_actuel) <- true;
    if pt_actuel = num_arrivee then begin
      meilleur_distance := !distance;
      meilleur_chemin := chemin.(!indice) <- pt_actuel
      end
    else
      let x_pt, y_pt = coords.(pt_actuel) in
      let coeff = (y_arrivee-y_pt)/(x_arrivee-x_pt) in 
      let proches = proche (pt_actuel) coeff in 
      fonction_bla_bla (voisins_les_plus_proches.(0)) ;
      while !chemin = None do 
        fonction_bla_bla (voisins_les_plus_proches.(!autre_indice));
        autre_indice := !autre_indice +1;
      end;
      !chemin.(!indice) <- pt_actuel
