(*Liste d'adjacence non pondérée*)
let adj2 = [|
  [1];
  [3];
  [0;1];
  [];
  [2;3;5];
  [3;6];
  [];
|]


let transpose graphe = 
  let tab = Array.make (Array.length graphe) [] in
  let rec parcours liste indice =
    match liste with
    | h :: t -> (tab.(h) <- indice :: tab.(h);
                 parcours t indice;)
    |[] -> []
  in

  for k = 0 to (Array.length tab) - 1 do
    parcours graphe.(k) k;
  done;
  tab;;


type 'a position = {
  etat : 'a;
  joueur : bool;
}

type 'a jeu = {
init:'a position;
va:'a position list;
vb:'a position list;
nul:'a position list;
coups:'a position -> 'a position list;
}

let dict_positions (j:'a jeu) =
  let t = Hashtbl.create(1000) in
  Hashtbl.add t j.init true;
  let rec parcours p =
    let ajout s = if not(Hashtbl.mem t s) then (Hashtbl.add t s true; parcours s)
    in List.iter ajout (j.coups p);
  in
  parcours (j.init);
  t ;;