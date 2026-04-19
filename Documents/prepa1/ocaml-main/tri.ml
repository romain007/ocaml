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


let division liste =
  let n = List.length liste / 2 in
  let rec div liste n = 
    match liste with
    | [] -> ([], []) 
    | t::s -> if n = 0 then ([],t::s) (* si n = 0, première moitié vide, tout dans la seconde *)
        else
          let left, right = div (s) (n-1) in
          (t :: left, right)
  in div liste n
;;

let rec fusion liste1 liste2 = 
  match liste1,liste2 with 
  |t1::s1,t2::s2 -> if t2 < t1 then t2 :: fusion (t1::s1) (s2)
      else t1 :: fusion (s1) (t2::s2)
  |[],l2 -> l2
  |l1,[] -> l1
    
;;

let rec tri_fusion liste = 
  let liste1,liste2 = division(liste) in
  if List.length liste1 = 0 || List.length liste2 = 0 then fusion liste1 liste2
      
  else
    fusion (tri_fusion liste1) (tri_fusion liste2)
    
;;