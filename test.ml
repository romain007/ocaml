(* Définition du type file_prio pour représenter les files de priorités, et
les opérations associées *)
let adj = [|
  [(1,1);(2,2)];
  [(0,1);(3,2);(5,3)];
  [(0,2);(3,3);(4,4)];
  [(1,2);(2,3);(4,2);(5,3)];
  [(2,4);(3,2);(6,5)];
  [(1,3);(3,3);(6,4)];
  [(3,3);(4,5);(5,4)] 
|]


type file_prio = (int*int) list ;;

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


let rec appartient t1 l =
  match l with
  |[] -> false
  |(e1,e2)::s -> if e1 = t1 then true else appartient t1 s;;

let rec float_assoc a1 l =
  match l with
  | [] -> failwith "file vide"
  | (e1,e2)::s -> if e1 = a1 then e2 else float_assoc a1 s ;;

let valeur opt =
  match opt with
  | Some x -> x     
  | None -> 0
    
let dijkstra (graphe,s) =
  let n = Array.length(graphe) in
  let distance = Array.make n None in 
  let pred = Array.make n 0 in
  let filep = ref [] in

  distance.(s) <- Some(0);

  for i = 0 to n -1 do 
    filep := ajout_file_prio (!filep) (i,valeur(distance.(i)))  ;
  done; 

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
            filep := ajout_file_prio (!filep) (t,valeur(distance.(t)))
          end ;
        parcour(s,u);

    |[] -> ()
           
  in
  
  while !filep <> [] do
    let u,poids_u = premier_elt_file_prio(!filep) in
    parcour (graphe.(u),u);
  done;;
                 
                 
dijkstra(adj,0);
