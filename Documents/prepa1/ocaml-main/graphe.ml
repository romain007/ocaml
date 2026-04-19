(*git pull origin main
git add .
git commit -m "Résolution des conflits"
git push origin main
*)



(*Liste d'adjacence non pondérée*)
let adj2 = [|
  [];
  [2];
  [4];
  [1;2];
  [];
  [3;4;6];
  [4,7];
  [];
|]

(*Liste d'adjacence pondérée*)
let (g)= [|
  [(1,1);(2,2)];
  [(0,1);(3,2);(5,3)];
  [(0,2);(3,3);(4,4)];
  [(1,2);(2,3);(4,2);(5,3)];
  [(2,4);(3,2);(6,5)];
  [(1,3);(3,3);(6,4)];
  [(3,3);(4,5);(5,4)] 
|]
(*Matrice d'adjacence pondérée*)
let mat_adj = [|
  [|  infinity; 7.; 12.; infinity; infinity |];  (* A *)
  [| infinity; infinity; 4.; 8.; infinity |];   (* B *)
  [| infinity; infinity; infinity; 13.; 5. |];  (* C *)
  [| infinity; infinity; infinity; infinity; 6. |];   (* D *)
  [| infinity; infinity; infinity; infinity; infinity |];   (* E *)
|];;

(*PILE *)
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
  s.data <- e::(s.data); 
  s.size <- s.size+1 ;;
let stack_top (s:'a stack) =
  List.hd (s.data) ;;
let stack_pop (s:'a stack) =
  match s.data with
  | t::q -> (s.data<-q ; s.size <- s.size-1; t);;

let stack_iter f s =
  List.iter f s.data
;;

(*FILE PRIORITAIRE*)
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



let parcours_largeur graphe init = 
  let file = Queue.create () in
  let etats = Array.make (Array.length graphe) false in 
  let resultat = ref [] in
  Queue.push init file;
  
  
  while not (Queue.is_empty file)  do
    afficher_file_int file;
    let elem = Queue.take file in
    if not etats.(elem) then  
      begin 
        etats.(elem) <- true;
        resultat := elem :: !resultat;
        List.iter (fun x -> Queue.push x file) graphe.(elem)
      end
  done;
  List.rev !resultat;;

  
let parcours g u = 
  let visite = Array.make (Array.length g) false  in
  let resultat = ref [] in
  let pile = ref [] in
  pile := u::!pile;
  while !pile <> [] do
    let elem = List.hd !pile in
    pile := List.tl !pile;
    
    if not visite.(elem) then begin
      
      visite.(elem) <- true;
      resultat := elem :: !resultat; 
      List.iter (fun t -> if not visite.(t) then pile := t::!pile) g.(elem)
    end;
    
  done;
  List.rev !resultat;;

let parcours_profondeur graphe s =
  let n = Array.length graphe in
  let visited = Array.make n false in
  let resultat = ref [] in

  let rec dfs u =
    if not visited.(u) then begin
      visited.(u) <- true;
  
      (* List.iter remplace ta fonction explore_voisins *)
      List.iter dfs graphe.(u);
      resultat := u :: !resultat; 
    end
    
  in

  dfs s;
  List.rev !resultat


let tri_topo graphe = 
  let n = Array.length graphe in
  let visited = Array.make n false in
  let resultat = ref [] in
  
  let parcours_profondeur graphe s = 
  
    let rec dfs u =
      if not visited.(u) then begin
        visited.(u) <- true; 
      (* List.iter remplace ta fonction explore_voisins *)
        List.iter dfs graphe.(u);
        resultat := u :: !resultat; 
      end 
    in 
    dfs s;
    
  in
  
  for i = 0 to n-1 do 
    if not visited.(i) then  parcours_profondeur graphe i; 
  done;
  !resultat;;
        
let tri_topo2 graphe liste= 
  let n = Array.length graphe in
  let visited = Array.make n false in
  let resultat = ref [] in
  let resultat2 = ref [] in
  let final = Array.make n [] in
  
  let parcours_profondeur graphe s = 
  
    let rec dfs u =
      if not visited.(u) then begin
        visited.(u) <- true; 
      (* List.iter remplace ta fonction explore_voisins *)
        List.iter dfs graphe.(u);
        resultat := u :: !resultat; 
        resultat2 := u :: !resultat2; 
      end 
    in 
    dfs s;
    List.rev !resultat2
  in
  
  List.iter (fun i -> resultat2 := []; if not visited.(i) then final.(i) <- parcours_profondeur graphe i) liste; 
  
  final;;      
        
let transpose graphe = 
  let liste = Array.make  (Array.length graphe) [] in
  for i = 0 to (Array.length graphe)-1 do
    List.iter(fun element -> 
        liste.(element) <- i :: liste.(element)) graphe.(i);
  done;
  
  liste;;
  

let kosaraju graphe=
  let graphet = transpose graphe in 
  let liste = tri_topo graphe in
  tri_topo2 graphet liste;

let valeur opt =
  match opt with
  | Some x -> x     
  | None -> 0



(* Algorithme de Dijkstra sur un graphe pondéré représenté par une liste
d'adjacence *)

let dijkstra (g) s1 =

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

(* h est une fonction : sommet -> float *)
let a_star (g) s1 cible h =
  let n = Array.length g in 
  let distances = Array.make n None in (* Stocke g(s) *)
  let visite = Array.make n false in
  
  distances.(s1) <- Some(0.);

  let f = ref [] in
  (* IMPORTANT : La file de priorité trie selon g + h, 
     mais on stocke aussi g pour les calculs suivants *)
  f := ajout_file_prio (!f) (s1, 0. +. h s1);

  while (!f <> []) do
    (* t1 = sommet, t2 = f(t1) = g(t1) + h(t1) *)
    let (t1, f_score)::s = !f in
    f := s;

    (* Sortie anticipée si on a atteint la cible *)
    if t1 = cible then f := []; 
    
    if not visite.(t1) then begin
      visite.(t1) <- true;
      
      (* On retrouve g(t1) à partir du tableau des distances *)
      let Some(g_t1) = distances.(t1) in
      
      let rec mise_a_jour l1 =
        match l1 with
        | [] -> ()
        | (a1, poids_arc)::reste ->
            if not(visite.(a1)) then begin
              let nouveau_g = g_t1 +. poids_arc in
              let nouveau_f = nouveau_g +. h a1 in

              match distances.(a1) with
              | Some(ancien_g) when nouveau_g >= ancien_g -> () (* Pas mieux *)
              | _ -> 
                  (* Si c'est mieux ou si c'est la première fois qu'on voit a1 *)
                  if appartient a1 !f then f := suppr_elt_file_prio !f a1;
                  
                  distances.(a1) <- Some(nouveau_g);
                  f := ajout_file_prio !f (a1, nouveau_f)
            end;
            mise_a_jour reste
      in 
      mise_a_jour g.(t1);
    end
  done;
  distances ;;

let inf = max_int;;
let test = [|
  [| 0;  3;  inf;  7 |]; (* Sommet 0 *)
  [| 8;  0;  2;  inf |]; (* Sommet 1 *)
  [| 5;  inf;  0;  1 |]; (* Sommet 2 *)
  [| 2;  inf;  inf;  0 |]  (* Sommet 3 *) |];;
  
  
let warshall graphe = 
  let n = Array.length graphe in
  let next = Array.make_matrix n n None in
  (*Initialise tableau next*)
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      if graphe.(i).(j) <> inf then next.(i).(j) <- Some(j)
    done;
  done;
  
  for k = 0 to n-1 do
    for i = 0 to n-1 do
      for j = 0 to n-1 do
        
        if graphe.(i).(k) <> inf && graphe.(k).(j) <> inf then 
          if graphe.(i).(j) > graphe.(i).(k) + graphe.(k).(j) then
            begin
              graphe.(i).(j) <- graphe.(i).(k) + graphe.(k).(j);
              next.(i).(j) <- next.(i).(k)
            end
      done;
    done;
  done;
  graphe, next
  

let (g)= [|
  [(1,1);(2,2)];
  [(0,1);(3,2);(5,3)];
  [(0,2);(3,3);(4,4)];
  [(1,2);(2,3);(4,2);(5,3)];
  [(2,4);(3,2);(6,5)];
  [(1,3);(3,3);(6,4)];
  [(3,3);(4,5);(5,4)] 
|]
let g = [|
  [(1,5); (4,1); (3,4)];  (* A = 0 *)
  [(0,5); (2,4); (3,2)];  (* B = 1 *)
                          
  [(1,4); (7,4); (8,1); (9,2)]; (* C = 2 *)
                                
  [(0,4); (1,2); (4,2); (7,2); (6,11)]; (* D = 3 *)
                                        
  [(0,1); (3,2); (5,1)]; (* E = 4 *)
                         
  [(3,5); (4,1); (6,7)]; (* F = 5 *)
                         
  [(3,11); (5,7); (7,1)]; (* G = 6 *)
                          
  [(2,4); (3,2); (6,1); (8,6)]; (* H = 7 *)
                                
  [(2,6); (6,4); (7,6); (9,0)]; (* I = 8 *)
  [(2,2); (8,0)]; (* J = 9 *)
|]

let tri_arete liste_adjacence = 
  let n = Array.length liste_adjacence in 
  let aretes = ref [] in
  let w = [|0;0;max_int|] in
  let aretes_trie = ref [] in
  let rec taille liste = 
    match liste with 
    |t::s -> 1 + taille s 
    |[] -> 0
  in
  
      
  (* Transforme en liste des arètes*)
  let f i elem = 
    let j,p = elem in
    aretes := (i,j,p) :: !aretes
  in 
  for i = 0 to n-1 do
    let g = f i in
    List.iter g liste_adjacence.(i)
  done;

  let sort liste  = 
    let rec min liste= 
      match liste with
      |(e1,e2,p) :: s -> 
          if p < w.(2) then 
            begin 
              w.(2) <- p;
              w.(1) <- e2;
              w.(0) <- e1
            end;
          min s
      |[] -> ()
    in
    let rec supr e t liste = 
      match liste with 
      |(e1,e2,p1) :: s -> if e1 = e && e2 = t then s
          else
            (e1,e2,p1) :: supr e t s
      |[] -> []

    in
    let nn = taille(!aretes)-1 in
    for i = 0 to nn do
      
      min !aretes; 
      aretes_trie := (w.(0),w.(1),w.(2))  :: !aretes_trie;
      aretes := supr w.(0) w.(1) !aretes; 
      w.(2) <- max_int;
    done;
  in sort !aretes;
  
  !aretes_trie
;;

let triee_par_poids = 
  List.sort (fun (_, _, p1) (_, _, p2) -> compare p1 p2) liste_aretes;;

let kruskal graphe = 
  let aretes_trie = tri_arete graphe in
  List.iter (fun x -> let e1,e2,e3 = x in Printf.printf "Aretes numero  : (%d,%d,%d)\n"e1 e2 e3 ) !aretes_trie;

  let n = List.length !aretes_trie in
  let n2 = Array.length graphe in
  let aretes_final = ref [] in
  let dico = Hashtbl.create n2 in
  for i = 0 to n2-1 do
    Hashtbl.add dico i i
  done;
  let remplace value_cible nouvelle_valeur= 
    Hashtbl.iter (fun key value ->
        if value = value_cible then
          Hashtbl.replace dico key nouvelle_valeur
      ) dico
  in
  
  let parcours elem = 
    let e1,e2,p = elem in 
    let value1 = Hashtbl.find dico e1 in
    let value2 = Hashtbl.find dico e2 in
    if value1 <> value2 then (*METHODE FIND*)
      begin
        
      (*On remplace la couleur du groupe de e1 par la couleur du groupe de e2*) 
        Hashtbl.replace dico e1 value2; 
        remplace value1 value2; (*METHODE UNION*)
        aretes_final := elem :: !aretes_final
      end
  in
  List.iter parcours !aretes_trie;
  !aretes_final
  



    





type uf = { parent : int array; rang : int array }

(* Initialisation : chaque élément est son propre chef *)
let creer_uf n =
  { parent = Array.init n (fun i -> i);
    rang = Array.make n 0 }

(* FIND avec compression de chemin *)
let rec find uf i =
  if uf.parent.(i) = i then
    i
  else begin
    (* On compresse : le parent devient la racine trouvée récursivement *)
    uf.parent.(i) <- find uf uf.parent.(i);
    uf.parent.(i)
  end

(* UNION par rang *)
let union uf i j =
  let root_i = find uf i in
  let root_j = find uf j in
  if root_i <> root_j then begin
    if uf.rang.(root_i) < uf.rang.(root_j) then
      uf.parent.(root_i) <- root_j
    else if uf.rang.(root_i) > uf.rang.(root_j) then
      uf.parent.(root_j) <- root_i
    else begin
      uf.parent.(root_i) <- root_j;
      uf.rang.(root_j) <- uf.rang.(root_j) + 1
    end;
    true (* Fusion effectuée *)
  end 
  else
    false (* Déjà dans le même groupe *)

let kruskal adj =
  let n = Array.length adj in
  let liste_aretes = ref [] in
  for i = 0 to n-1 do 
    List.iter (fun (s, p) -> if i < s then liste_aretes := (i, s, p) :: !liste_aretes ) adj.(i)
  done;
      
  (* 1. Trier les arêtes par poids croissant *)
  let aretes_triees = List.sort (fun (_, _, p1) (_, _, p2) -> compare p1 p2) !liste_aretes in
  
  (* 2. Initialiser l'Union-Find et la liste du résultat *)
  let uf = creer_uf n in
  let liste = ref [] in
  
  (* 3. Parcourir les arêtes *)
  List.iter (fun (u, v, p) ->
    (* Si l'union réussit, c'est que u et v n'étaient pas connectés *)
      if union uf u v then
        liste := (u, v, p) :: !liste
    ) aretes_triees;
  
  !liste (* Retourne l'Arbre Couvrant Minimum *)