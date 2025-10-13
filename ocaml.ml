(*git pull origin main
git add .
git commit -m "Résolution des conflits"
git push origin main
*)



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

let p = stack_create();;
stack_push 3 p;;
stack_push 4 p;;
stack_push 5 p;;

let k = stack_top p;;
stack_pop p;;
let k = stack_top p;;

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


let parcours_profondeur (graphe, s) =
  let etats = Array.make (Array.length graphe) false in
  let l = Queue.create () in
  let rec parcours_profondeur2 (graphe, s) =
    etats.(s) <- true;

    let rec parcour liste =
      match liste with
      | t :: r ->
          if not etats.(t) then (
            etats.(t) <- true;
            Queue.push t l;
            Printf.printf "\nOn rencontre l'element %d\n" t;
            parcours_profondeur2 (graphe, t);
            parcour r
          ) else
            parcour r
      | [] -> ()
    in
    parcour graphe.(s)
  in
  parcours_profondeur2 (graphe, s);
  let li = queue_to_list l in 
  li
;;


let tri_topologique (graphe) =
  let etats = Array.make (Array.length graphe) false in 
  let tri = stack_create() in 
  
  let rec tri_topo (graphe,i)= 
    let parcour = parcours_profondeur(graphe,i) in 
    let rec verif (parcour,i) = 
      match parcour with
      |t::s -> if not etats.(t) then 
            begin
              etats.(t) <- true; 
              Printf.printf "\nOn fait le tri topo sur  %d\n" t;
              tri_topo(graphe,t);
              verif (s,i);
            end
          else
            begin
              verif (s,i);
              Printf.printf "\nOn fait la vérif pour  %d\n" t;
            end
      |[] -> 
          stack_push i tri ;
          Printf.printf "\nOn ajoute la val à la pile %d\n" i;
          
    in
    verif (parcour,i);
  
  in 
  for i = 0 to  Array.length graphe - 1 do
    Printf.printf "\nOn fait le tri topo numero %d\n" i;
    tri_topo(graphe,i); 
  done;
    
  tri
;;

tri_topologique(graphe);



(*parcours_largeur(adj2,4);*)