(* Tri par selection*)


let rec supr elem liste = 
  match liste with 
  |t::s -> if t = elem then s
      else
        t :: supr elem s
  |[] -> []

let min liste = 
  let w = List.hd liste in
  let rec minimum liste w = 
    match liste with
    |t::s -> if t < w then minimum s t
        else minimum s w
    |[] -> w
  in minimum liste w;;


let rec tri_selection liste = 
  match liste with
  | [] -> []
  | t::s ->
      let m = min liste in
      m :: tri_selection (supr m liste) 
  

let rec insere x liste =
  match liste with
  | [] -> [x]                      (* si la liste est vide, on insère x *)
  | t::s -> if x <= t then x :: t :: s 
      else
        t :: insere x s
;;


let rec tri_insertion liste =
  match liste with
  | [] -> []                      (* liste vide → triée *)
  | t::s -> insere t (tri_insertion s)
;;

let rec bubble_pass l =
match l with
| [] -> []
| [x] -> [x]
| x :: y :: rest ->
    if x > y then y :: bubble_pass (x :: rest)
    else x :: bubble_pass (y :: rest)

let rec tri_bulle l =
  match l with
  | [] -> []
  | _ ->
      let l' = bubble_pass l in
      match List.rev l' with
      | [] -> []
      | max :: rest_rev -> tri_bulle (List.rev rest_rev) @ [max]


;;