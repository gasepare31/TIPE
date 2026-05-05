let add_one (l : bool list) : bool list =
  let rec add_one_rev (l : bool list) : bool list =
    match l  with
    | [] -> failwith "liste vide"
    | x::q -> if x=false then true::q 
        else false :: add_one_rev q
  in List.rev (add_one_rev (List.rev l))



let rec valuation_next (sigma : valuation) : valuation option =




let test() =
  (* test add_one *)
  assert(add_one [true; false ; false; true; true]=[true; false; true; false; false]);
  assert(add_one [true; false; false; true; false] = [true; false; false; true; true]);

  print_string "Tous les tests ont réussi !"