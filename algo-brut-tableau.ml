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

type monument = (int * float) list * (float * float) (* est-ce qu'on met qdmm le 
   numéro du monument considéré ou ce sera juste l'indice ? *)

(* renvoie l'indice du nom *)
let nom_to_num (nom : string) (noms : string array) : int  = 
  let result = ref (-1) in
  Array.iteri (fun i x -> if x = nom then result := i) noms;
  if !result = -1 then failwith "nom pas dans la liste"
  else !result

(* Compare deux chemins optionnels et retourne le plus court.
   Si l'un des deux est None (inexistant), retourne l'autre.
   Si les deux sont None, retourne None. *)
let meilleur_chemin (c1 : (int list * float)option ) (c2 : (int list * float) option) : (int list * float) option= 
   match c1, c2 with 
   | Some (chem1, dist1), Some (chem2, dist2) -> if dist1 < dist2 then c1 else c2
   | Some (chem1, dist1), None -> c1
   | None, Some (chem1, dist1) -> c2
   | None, None -> None

let chemin_force_brute (depart : string) (arrivee : string) : string list * float =
 (* Cherche récursivement un chemin depuis pt_actuel jusqu'à id_arrivee.
   visite contient les points déjà visités pour éviter les cycles.
   Retourne Some (chemin, distance_totale) si un chemin existe, None sinon.*)

  let num_depart = nom_to_num depart noms in
  let num_arrivee = nom_to_num arrivee noms in

  let chemin = ref (Array.make n (-1)) in
  !chemin.(0) <- num_depart;
  let distance = ref 0. in
  let indice = ref 1 in

  let meilleur_chemin = ref [||] in
  let meilleur_distance = ref Float.infinity in

  let visite = ref (Array.make n false) in 

  let rec explore (pt_actuel : int) : unit =
    (* une fois arrivé à la destination souhaitée, on compare la distance totale avec la meilleur distance en mémoire, et si elle est meilleure, on la remplace, et le chemin aussi*)
    if pt_actuel = num_arrivee then begin
      if !distance < !meilleur_distance then begin 
        meilleur_distance := !distance;
        !chemin.(!indice) <- pt_actuel;
        meilleur_chemin := Array.sub !chemin 0 (!indice) (* recrée un tableau à partir de !chemin, allant de l'indice 0 à !indice*)
      end
    end
    (* si on est pas sur la destination souhaitée *)
    else if not !visite.(pt_actuel) then begin
      !visite.(pt_actuel) <- true; (* on dit qu'on a visité le pt actuel *)
      let list_voisins = voisins.(pt_actuel) in (* on récupère la liste des voisins du point actuel, sous forme d'une liste de tuples avec indice et distance *) 
      for i = 0 to Array.length list_voisins - 1 do (* pour chaque voisin *)
        let voisin_idx, dist = list_voisins.(i) in (* on sépare l'indice du voisin et sa distance dans deux variables *)
        distance := !distance +. dist; (* on actualise la distance du chemin parcouru *)
        !chemin.(!indice) <- voisin_idx; (* on ajoute le voisin au chemin parcouru *)
        indice := !indice + 1; (* l'indice augmente parce qu'on a ajouté un point au chemin *)
        explore voisin_idx; (* enfin la RÉCURSIVITÉ !! On explore ensuite à partir de ce voisin *)
        indice := !indice - 1; (* une fois fini, si le chemin était plus court que meilleur_chemin, il aura été actualisé, mais comme on va passer à l'autre voisin, il faut retirer cette ville de notre chemin, pour cela on réduit l'indice, comme ca quand on va ajouter la prochaine ville, ça va l'écraser *)
        distance := !distance -. dist (* et pour les mêmes raisons on soustrait la distance *)
      done;
      !visite.(pt_actuel) <- false 
    end
  in

  explore num_depart ;
  if !meilleur_distance = Float.infinity then
    ([], Float.infinity)
  else
  let chemin_noms = Array.map (fun idx -> noms.(idx)) !meilleur_chemin in
  (Array.to_list chemin_noms, !meilleur_distance)


(* TEST DE L'ALGO *)

(* avec les calculs de flottants, les résultats sont pas exactement ceux attendus, 
   mais on vérifie qu'ils sont corrects à 0.01 près*)
let approx a b = abs_float (a -. b) < 0.01

(* test sur des exemples précis *)
let test ()=
  let (chemin, dist) = chemin_force_brute "Trocadéro" "Tour Eiffel" in
    assert (chemin = ["Trocadéro"; "Tour Eiffel"]);
    assert (dist = 0.7);
  
  let (chemin, dist) = chemin_force_brute "Panthéon" "Musée d'Orsay" in
    assert (approx dist 1.8);
    assert(chemin = ["Panthéon"; "Jardins du Luxembourg"; "Musée d'Orsay"]);

  print_string "Bravo :)"

