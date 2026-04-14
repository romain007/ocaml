type 'a btree = Vide | Noeud of 'a * 'a btree * 'a btree

(*arbre 2-3*)
type 'a btree23 = Vide | Noeud1 of 'a * 'a btree23 * 'a btree23 | Noeud2 of 'a * 'a btree23 * 'a btree23

(*Ou alors*)
(*type 'a noeud = {
  valeur : 'a;
  gauche : 'a btree;
  droite : 'a btree;
}
and 'a btree = { 
  contenu : 'a noeud option 
}
let mon_arbre = { 
  contenu = Some { 
    valeur = 4; 
    gauche = { contenu = None }; 
    droite = { contenu = None } 
  } 
} *)
 
let mon_arbre = Noeud (4, Noeud (3, Vide, Vide), Vide)  (*Spécifie pas l'objet, ocaml comprend lui meme que c'est btree au vue des constructeurs utilisés*)
let mon_arbre : int btree = Noeud (4, Noeud (3, Vide, Vide), Vide) (*On spécifie que on crée un objet btree*)
let mon_arbre : int btree = Noeud 
    (1, Noeud 
       (2, Noeud (4, Vide, Vide), Noeud (5, Vide, Vide)), 
     Noeud (3, Vide, Vide)) 

(* Arbre d'arité non définis*)
type 'a btree = Noeud of 'a * 'a btree list



let rec parcours_infixe arbre =
  match arbre with
  | Vide -> []
  | Noeud(v, g, d) -> (parcours_infixe g) @ [v] @ (parcours_infixe d)

let rec parcours_prefixe arbre =
  match arbre with
  | Vide -> []
  | Noeud(v, g, d) -> [v] @ (parcours_prefixe g) @ (parcours_prefixe d)

let rec parcours_postfixe arbre =
  match arbre with
  | Vide -> []
  | Noeud(v, g, d) -> (parcours_postfixe g) @ (parcours_postfixe d) @ [v]
                                                  
let parcours_largeur arbre = 
  let resultat = ref [] in 
  let q = Queue.create() in

  Queue.push arbre q;

  while not (Queue.is_empty q) do 
    let u = Queue.take q in 
    
    match u with
    |Vide -> ()
    |Noeud(valu,g,d) -> 
        Queue.add g q;
        Queue.add d q;
        resultat := valu :: !resultat; 
  
  done;
  List.rev !resultat;;

(*Parcours sans le @ optimisé avec accumulateur*)
let parcours_infixe2 arbre =
  let rec parcours acc a = 
    match a with
    | Vide -> acc
    | Noeud(v, g, d) ->
        (* 1. On remplit d'abord l'accumulateur avec la partie droite *)
        let acc_droite = parcours acc d in
        (* 2. On ajoute la racine actuelle au milieu *)
        let acc_avec_v = v :: acc_droite in
        (* 3. On finit par remplir avec la partie gauche *)
        parcours acc_avec_v g
  in 
  parcours [] arbre
    
let parcours_prefixe2 arbre =
  let rec aux acc a =
    match a with
    | Vide -> acc
    | Noeud(v, g, d) ->
        let acc_droite = aux acc d in
        let acc_gauche = aux acc_droite g in
        v :: acc_gauche
  in 
  aux [] arbre
    

let parcours_postfixe2 arbre =
  let rec aux acc a =
    match a with
    | Vide -> acc
    | Noeud(v, g, d) ->
        let acc_avec_v = v :: acc in
        let acc_droite = aux acc_avec_v d in
        aux acc_droite g
  in 
  aux [] arbre

let rec recherche arbre x = 
  match arbre with
  |Vide -> false 
  |Noeud(v,g,d) -> if x < v then recherche g x
                  if x> v then recherche d x
                  else true

type tas = {
  mutable size : int;
  data : int array;
}

(* Initialise un tas avec une capacité maximale fixe *)
let create capacity = 
  { size = 0; data = Array.make capacity 0 }

let tas_binaire = [||]

let ajout tas e = 

  let i = tas.size -1 in  
  tas.data.(i) <- e;
  tas.size <- tas.size + 1;

  while tab.data.(i-1/2) > tab.data.(i) do 

    let z = tab.data.(i) in
    tab.data.(i) <- tab.data.(i-1/2);
    tab.data.(i-1/2) <- z;

let retirer tas =
  let value = tas.data.(0)
  tas.size <- tas.size - 1
  (*On remplace par la racine le dernier element*)
  tas.data.(0) <- tas.data.(size -1)
  let i = 0 in
  while (tab.data.(i) > tas.data.(2i+1)) || (tab.data.(i)> tab.data(2i+2)) do
    if tab.data.(i) > tas.data.(2i+1) then let z = tab.data.(i) in
                                           tab.data.(i) <- tab.data.(2i+1);
                                           tab.data.(2i+1) <- z;
    else if tab.data(i) > tab.data(2i+2) then let z = tab.data.(i) in
                                           tab.data.(i) <- tab.data.(2i+2);
                                           tab.data.(2i+2) <- z;



(*TAS BINAIRE*)

type tas = {
  mutable size : int;
  data : int array;
}

(* Initialise un tas avec une capacité maximale fixe *)
let create capacity = 
  { size = 0; data = Array.make capacity 0 }

let tas = create 100;;

let ajout tas e = 
  if tas.size == 0 then 
    begin
      tas.data.(0) <- e;
      tas.size <- tas.size + 1
    end
  else
    let i = ref (tas.size ) in  
    tas.data.(!i) <- e;
    tas.size <- tas.size + 1;

    while tas.data.((!i-1)/2) > tas.data.(!i) do 

      let z = tas.data.(!i) in
      tas.data.(!i) <- tas.data.((!i-1)/2);
      tas.data.((!i-1)/2) <- z;
      i := (!i -1) /2
    done
;;

      
      
let retirer tas = 
  let value = tas.data.(0) in
  
  
  if tas.size > 0 then begin
    tas.data.(0) <- tas.data.(tas.size-1);
    tas.data.(tas.size-1) <- 0;
    tas.size <- tas.size - 1;
    let i = ref 0 in
    let finish = ref false in
    
    while not !finish do
      let g = 2 * !i + 1 in
      let d = 2 * !i + 2 in
      let plus_petit = ref !i in
      
      (* 1. On cherche l'indice du plus petit parmi {parent, filsG, filsD} *)
      if g < tas.size && tas.data.(g) < tas.data.(!plus_petit) then
        plus_petit := g;
      if d < tas.size && tas.data.(d) < tas.data.(!plus_petit) then
        plus_petit := d;
      
      (* 2. On n'échange qu'une seule fois si nécessaire *)
      if !plus_petit <> !i then begin
        let z = tas.data.(!i) in
        tas.data.(!i) <- tas.data.(!plus_petit);
        tas.data.(!plus_petit) <- z;
        i := !plus_petit
      end
      else
        finish := true
    done
  end;
  value;;