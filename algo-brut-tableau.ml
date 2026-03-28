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
let nom_to_num (nom : string) (noms : string array) : int option = 
  Array.find_index (fun x -> x = nom) noms (* find_index revoie le rang du premier element qui satisfait la fonction, ici x -> x = nom *)

let chemin_force_brute (map : monument list) (depart : monument) (arrivee : monument) : string list * float =
  