(* algo brut prends en entrée la liste de tous les chemins, un point de départ, un pt d'arrive, 
  puis renvoie le meilleur chemin (le plus court) *)

(*let big_list = ["Tour Eiffel"]*)

type monument = string * (string * float) list * (float * float)

let monuments : monument list = [
  ("Tour Eiffel",
   [("Musée d'Orsay", 2.2); ("Champ-de-Mars", 0.4); ("Trocadéro", 0.7)],
   (48.8584, 2.2945));

  ("Arc de Triomphe",
   [("Champs-Élysées", 1.1)],
   (48.8738, 2.2950));

  ("Champs-Élysées",
   [("Arc de Triomphe", 1.1); ("Place de la Concorde", 1.0)],
   (48.8698, 2.3078));

  ("Place de la Concorde",
   [("Champs-Élysées", 1.0); ("Musée d'Orsay", 0.7); ("Louvre", 1.2)],
   (48.8656, 2.3212));

  ("Musée d'Orsay",
   [("Tour Eiffel", 2.2); ("Place de la Concorde", 0.7); ("Louvre", 1.4); ("Jardins du Luxembourg", 1.2)],
   (48.8600, 2.3266));

  ("Louvre",
   [("Musée d'Orsay", 1.4); ("Place de la Concorde", 1.2); ("Palais-Royal", 0.2)],
   (48.8606, 2.3376));

  ("Palais-Royal",
   [("Louvre", 0.2); ("Sacré-Cœur", 3.1)],
   (48.8638, 2.3370));

  ("Sacré-Cœur",
   [("Palais-Royal", 3.1)],
   (48.8867, 2.3431));

  ("Notre-Dame",
   [("Panthéon", 1.2); ("Place de la Bastille", 2.1)],
   (48.8530, 2.3499));

  ("Panthéon",
   [("Notre-Dame", 1.2); ("Jardins du Luxembourg", 0.6)],
   (48.8462, 2.3460));

  ("Jardins du Luxembourg",
   [("Panthéon", 0.6); ("Musée d'Orsay", 1.2)],
   (48.8462, 2.3372));

  ("Place de la Bastille",
   [("Notre-Dame", 2.1)],
   (48.8533, 2.3692));

  ("Trocadéro",
   [("Tour Eiffel", 0.7)],
   (48.8625, 2.2893));

  ("Champ-de-Mars",
   [("Tour Eiffel", 0.4)],
   (48.8556, 2.2986));
]

(* renvoie le nbre de liens qu'un monument a *)
let nbre_liens (m : monument) : int = 
  let id, voisins, coord = m in 
  let rec len (l : (string * float) list ) : int = 
    match l with
    | x :: q -> 1 + len q
    | [] -> 0 
  in len voisins


(* prends un nom et renvoie le monument associé *)
let rec nom_to_monument (nom : string) (liste : monument list): monument = 
  match liste with
  | (id, voisins, coord) :: q -> if id = nom then (id, voisins, coord) else nom_to_monument nom q
  | [] -> failwith "monument non existant"

(* prends en entrée le point de départ, et le point d'arrivée et la liste des monuments, renvoie le meilleur chemin et sa distance *)
let chemin_force_brute (map : monument list) (depart : monument) (arrivee : monument) : string list * float =
  (* récupère l'identifiant des points de départ et d'arrivée *)
  let id_depart, _, _ = depart in 
  let id_arrivee, _, _ = arrivee in 

  let meilleur_chemin (c1 : (string list * float)option ) (c2 : (string list * float) option) : (string list * float) option= 
    match c1, c2 with 
    | Some (chem1, dist1), Some (chem2, dist2) -> if dist1 > dist2 then c1 else c2
    | Some (chem1, dist1), None | None, Some (chem1, dist1) -> c1
    | None, None -> None
  in

  let rec chemin_pt (pt_actuel : string) (visite : string list) : (string list * float) option = 
    if pt_actuel = id_arrivee then Some ([], 0.0)
    else 
      let _, voisins, _ = nom_to_monument pt_actuel map in 
      explore_voisins pt_actuel voisins (pt_actuel :: visite) 

  and explore_voisins (pt_actuel : string) (voisins : (string * float) list) (visite : string list) : (string list * float) option = 
    match voisins with
    | [] -> None
    | (voisin, distance) :: q -> if List.mem voisin visite (* list.mem revoie true si voisin est dans visite *)
      then explore_voisins pt_actuel q visite
      else
         match chemin_pt voisin visites with 
         








