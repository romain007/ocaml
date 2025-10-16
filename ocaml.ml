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

let parcours_largeur(graphe,s) =
  
  (*Liste d'etats pour chaque elements du graphe*)
  let etats = Array.make (Array.length graphe) false in
  let q = Queue.create () in
  let resultat = ref [] in

  etats.(s) <- true;

  (*Commence à l'element s avec une file*)
  Queue.push s q ;
  
  while not (Queue.is_empty q)  do

    let k = Queue.take q in 
    resultat :=  !resultat @ [k];
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
            end;
          parcour s;

      |[] -> ()
    in parcour graphe.(k)
  done;
  !resultat;;

  let parcours_profondeurp (graphe,s) =
  let n = Array.length graphe in
  let etats = Array.make n false in
  let p = stack_create () in
  let resultat = ref [] in

  etats.(s) <- true;
  stack_push s p;

  while not (stack_is_empty p) do
    let k = stack_pop p in
    resultat := !resultat @ [k];

    (* On parcourt les voisins de k en ordre inverse pour bien respecter le DFS *)
    let voisins = List.rev graphe.(k) in
    let rec parcour liste = 
      match liste with
      | [] -> ()
      | t :: reste ->
          if not etats.(t) then 
            begin
              etats.(t) <- true;
              stack_push t p;
            end;
          parcour reste
    in
    parcour voisins
  done;

  !resultat
;;

let parcours_profondeur (graphe,s) =
  let n = Array.length graphe in
  let visited = Array.make n false in
  let resultat = ref [] in

  (* Fonction récursive interne *)
  let rec parcour_profondeur2 u =
    visited.(u) <- true;
    resultat := !resultat @ [u];  (* on ajoute u à la liste des visités *)

    (* Parcours explicite des voisins *)
    let rec explore_voisins voisins =
      match voisins with
      | [] -> ()
      | v :: reste ->
          if not visited.(v) then parcours_profondeur2 v;
          explore_voisins reste
    in
    explore_voisins graphe.(u)
  in

  parcours_profondeur2 s;
  !resultat  (* retourne la liste dans l'ordre de visite *)
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

  !tri  (* retourne la liste dans l'ordre de visite *)
;;

let valeur opt =
  match opt with
  | Some x -> x     
  | None -> 0



let dijkstra2 (graphe,s) =
  let n = Array.length(graphe) in
  let distance = Array.make n None in 
  let pred = Array.make n 0 in
  let filep = ref [] in

  distance.(s) <- Some(0);
  
  (* On ajoute le premier élèment dans la file *)
  filep := ajout_file_prio (!filep) (0,valeur(distance.(s)))  ;
  

  (*Fonction qui parcours tous les sommets adjacent à u. 
    Si la distance du sommet t est supérieur à celle de u vers t,
    on attribue à t la bonne distance de u à t
    et on attribue comme prédecesseur à t la valeur u
    Puis on met à jour t dans sa file de priorité avec sa nouvelle distance*)
  
  let rec parcour (liste,u) = 
    match liste with
    |(t,poids_t)::s -> if valeur(distance.(t)) > valeur(distance.(u)) + poids_t then 
          begin
            distance.(t) <- Some(valeur(distance.(u)) + poids_t);
            pred.(t) <- u;
            filep := suppr_elt_file_prio (!filep) (
                filep := ajout_file_prio (!filep) (t,valeur(distance.(t)))
          end ;
        parcour(s,u);

    |[] -> ()
           
  in
  
  while !filep <> [] do
    let u,poids_u = premier_elt_file_prio(!filep) in
    parcour (graphe.(u),u);
  done;;


  (* Définition du type graphe_pondere représentant les graphes sous la forme
de listes d'adjacences *)

type graphe_pondere = (int * float) list array ;;


(* Définition du type file_prio pour représenter les files de priorités, et
les opérations associées *)

type file_prio = (int*float) list ;;

let rec ajout_file_prio (l:file_prio) e =
  match l with
  | [] -> [e]
  | t::s -> let t1,t2 = t and e1,e2 = e in
      if e2 < t2 then e::l
      else t::(ajout_file_prio s e) ;;

let premier_elt_file_prio l =
  match l with
  | [] -> failwith "file vide"
  | t::s -> t ;;

let enlever_file_prio l =
  match l with
  | [] -> failwith "file vide"
  | t::s -> s ;;

let rec suppr_elt_file_prio l a =
  match l with
  | []-> failwith "file vide"
  | (t1,t2)::s -> if t1 = a then s else (t1,t2)::(suppr_elt_file_prio s a) ;;

let rec appartient t1 l =
  match l with
  |[] -> false
  |(e1,e2)::s -> if e1 = t1 then true else appartient t1 s;;

let rec float_assoc a1 l =
  match l with
  | [] -> failwith "file vide"
  | (e1,e2)::s -> if e1 = a1 then e2 else float_assoc a1 s ;;


(* Algorithme de Dijkstra sur un graphe pondéré représenté par une liste
d'adjacence *)

let dijkstra (g:graphe_pondere) s1 =

  let n = Array.length g in 
  
  (* On crée le tableau des distances au sommet initial, avec des éléments
  de type option : une distance infinie est représentée par None, une distance
  finie de valeur un flottant b est représentée par Some(b) ; les distances
  sont initialement infinies *)
  let distances = Array.make n None in
  
  (* On créele tableau visite, indiquant par un booléen pour chaque sommet
  s'il a été visité *)
  let visite = Array.make n false in
  distances.(s1) <- Some(0.);

  (* on crée une référence f vers une file de priorité *)
  let f = ref [] in
  f := ajout_file_prio (!f) (s1,0.);

  (* On fait défiler la file de priorité tant qu'elle est non-vide ; on ajoute
  ou modifie les distances associées aux voisins du sommet parcouru *)
  while (!f <> []) do
    
    (* on défile le premier élément de la file : t1 est le sommet, t2 est sa
    distance (réelle) au sommet initial associée, optimale car c'est le
    premier élément de la file *)
    let (t1,t2)::s = (!f) in
    
    (* On récupère la liste d'adjacence du sommet t1, contenue dans g.(t1) *)
    let lv = g.(t1) in 
    
    (* on enlève le premier élément de f *)
    f := s;
    
    (* on marque t1 comme visité *)
    visite.(t1) <- true;
    print_int t1;Printf.printf("\n");print_float t2;Printf.printf("\n"); 
    
    let rec mise_a_jour l1 =
      
      (* on définit récursivement la fonction mise_a_jour, qui prend en entrée
      une liste de couples sommets/longueurs d'arêtes (en pratique, c'est la
      liste d'adjacence du sommet t1) et, pour chaque couple dans la liste
      représentant un sommet s1 et la longueur de (t1,s1), met à jour la
      distance du sommet initial à s1 si nécessaire *)
      match l1 with
      | []-> ()
      | (a1,a2)::s ->
          begin
            if not(visite.(a1)) then (* On effectue la mise à jour seulement
                                     pour les sommet non-visités *)

              begin
                if appartient a1 (!f) then (* si le sommet a1 est dans la file
                                              de prio, sa distance au sommet
                                              initial est déjà <inf, on la
                                              modifie potentiellement ;
                                              sinon, on l'ajoute *)
                  begin
                    let Some(b2) = distances.(a1) in
                    if a2 +. t2 < b2 then
                        (* on supprime l'ancien couple (sommet,distance), on
                        ajoute le nouveau et on stocke la nouvelle distance dans
                        le tableau *)
                      begin
                        f := suppr_elt_file_prio (!f) a1;
                        f := ajout_file_prio (!f) (a1,a2 +. t2);
                        distances.(a1) <- Some(a2 +. t2);
                      end
                  end
                else (* Si l'élément n'est pas dans la file de prio, on fait
                     pareil mais sans la suppression *)
                  begin
                    f := ajout_file_prio (!f) (a1,a2 +. t2);
                    distances.(a1) <- Some(a2 +. t2);
                  end
              end;
            mise_a_jour s;
          end;
    in mise_a_jour lv;
    Printf.printf("\n\n");
  done;
  distances ;;

let (g:graphe_pondere)= [|
  [(1,1);(2,2)];
  [(0,1);(3,2);(5,3)];
  [(0,2);(3,3);(4,4)];
  [(1,2);(2,3);(4,2);(5,3)];
  [(2,4);(3,2);(6,5)];
  [(1,3);(3,3);(6,4)];
  [(3,3);(4,5);(5,4)] 
|]

let warshall (adj) = 
  let n = Array.length(adj) in
  let min a b = 
    if a = -1 || b = -1 then 
      if a = -1 then b 
      else a
  
    else
      begin 
        if 
          a<b then a
        else 
          b
      end
  
  in 
  
  let addition a b = 
    if a = -1 || b = -1 then 
      -1
    else
      a+b 
  in 
  
  
  for k = 0 to n-1 do
    for i = 0 to n-1 do
      for j = 0 to n-1 do 
        mat_adj.(i).(j) <- min (mat_adj.(i).(j)) (addition (mat_adj.(i).(k)) (mat_adj.(k).(j)) );
      done;
    done;
  done;
  
  mat_adj;;

let mat_adj = [|
  [|  -1; 7; 12; -1; -1 |];  (* A *)
  [| -1; -1; 4; 8; -1 |];   (* B *)
  [| -1; -1; -1; 13; 5 |];  (* C *)
  [| -1; -1; -1; -1; 6 |];   (* D *)
  [| -1; -1; -1; -1; -1 |];   (* E *)
|];;

  


    









