
let adj2 = [|
  [1];
  [3];
  [0;1];
  [];
  [2;3;5];
  [3;6];
  [];
|]


type 'a stack =
  {
    mutable data: 'a list;
    mutable size: int;
    
  } ;;

let stack_create () =
  {
    data = []; size = 0
  } ;;
let stack_is_empty (s:'a stack) =
  s.size = 0 ;;
let stack_push  (e:'a) (s: 'a stack) =
  s.data <- e::(s.data); s.size <- s.size+1 ;;
let stack_top (s:'a stack) =
  List.hd (s.data) ;;
let stack_pop (s:'a stack) =
  match s.data with
  | t::q -> (s.data<-q ; s.size <- s.size-1; t);;

let stack_iter f s =
  List.iter f s.data
;;

let queue_to_list q =
  Queue.fold (fun acc x -> acc @ [x]) [] q
;;



let parcours_largeur(graphe,s) =
  
  (*Liste d'etats pour chaque elements du graphe*)
  let etats = Array.make (Array.length graphe) false in
  etats.(s) <- true;
  let q = Queue.create () in
  
  (*Commence à l'element s avec une file*)
  Queue.push s q ;
  
  while not (Queue.is_empty q)  do
    let k = Queue.take q in 
    (*On regarde tous les voisins de k, en les marquant visité 
      puis en les ajoutant à la file*)
    let rec parcour liste =
      match liste with
      |t::s -> if not etats.(t)  then
            begin
              etats.(t) <- true;
              Printf.printf "On push l'element %d " t;
              Queue.push t q;
              parcour s;
            end
          else
            begin
              parcour s;
            end
      |[] -> ()
    in parcour graphe.(k)
  done;;
  
let parcours_profondeurp(graphe,s) =
  
  (*Liste d'etats pour chaque elements du graphe*)
  let etats = Array.make (Array.length graphe) false in
  etats.(s) <- true;
  let p = stack_create () in
  
  (*Commence à l'element s avec une file*)
  stack_push s p  ;
  
  while not (stack_is_empty p)  do
    let k = stack_top p in 
    stack_pop p ;
    Printf.printf "\n%d\n" k;
    (*On regarde tous les voisins de k, en les marquant visité 
      puis en les ajoutant à la pile*)
    let rec parcour liste =
      match liste with
      |t::s -> if not etats.(t)  then
            begin
              etats.(t) <- true;
              Printf.printf "On push l'element %d " t; 
              stack_push t p;
              parcour s;
            end
          else
            begin
              parcour s;
            end
      |[] -> ()
    in parcour graphe.(k)
  done;;



let parcours_profondeur (graphe,s) =
  let n = Array.length graphe in
  let visited = Array.make n false in
  let resultat = ref [] in

  (* Fonction récursive interne *)
  let rec dfs u =
    visited.(u) <- true;
    resultat := u :: !resultat;  (* on ajoute u à la liste des visités *)

    (* Parcours explicite des voisins *)
    let rec explore_voisins voisins =
      match voisins with
      | [] -> ()
      | v :: reste ->
          if not visited.(v) then dfs v;
          explore_voisins reste
    in
    explore_voisins graphe.(u)
  in

  dfs s;
  List.rev !resultat  (* retourne la liste dans l'ordre de visite *)
;;


let tri_topologique graphe =
  let n = Array.length graphe in
  let visited = Array.make n false in
  let tri = ref [] in

  (* Fonction récursive DFS *)
  let rec parcours_profondeur u =
    visited.(u) <- true;

    (* Parcours explicite des voisins de u *)
    let rec explore_voisins voisins =
      match voisins with
      | [] -> ()  (* plus de voisins *)
      | v :: reste ->
          if not visited.(v) then parcours_profondeur v;
          explore_voisins reste
    in

    explore_voisins graphe.(u);

    (* Une fois tous les voisins explorés, on empile u *)
    tri := u :: !tri
  in

  (* On lance DFS sur tous les sommets non visités *)
  for i = 0 to n - 1 do
    if not visited.(i) then parcours_profondeur i
  done;

  List.rev !tri  (* retourne la liste dans l'ordre de visite *)
;;

(*tri_topologique(adj2);*)
(*parcours_profondeur(adj2,4);*)
parcours_profondeurp(adj2,4);
(*parcours_largeur(adj2,4);*)
