(*IMMUABLE*)
(*On ne peut pas modifier la valeur des elements de la liste, c'est impossible*)

(*Liste chainée immuable*)
(*Les listes de ocaml*)
let liste = []

(*Pile immuable*)

(*On utilise une liste, push : s::pile, top : List.hd pile, pop : s::q*)
let pile = []

(*File immuable*)
(*On crée et renvoie une nouvelle file à chaque modification*)
(*Très lent*)
(*Soit on utilise une liste encore, push : file @ [s], top : List.hd file, pop : s::q*)

(*Plus rapide*)
(*Système avec double piles (ici donc liste)*)
type 'a file = {
  entree : 'a list;  (* éléments ajoutés récemment *)
  sortie : 'a list;  (* éléments à retirer en priorité *)
}


let file_vide = { entree = []; sortie = [] }
let est_vide f =
  f.entree = [] && f.sortie = []

let enfiler x f =
  { f with entree = x :: f.entree }  (* O(1) *)

let defiler f =
  match f.sortie with
  | t :: q -> (t, { f with sortie = q })
  | [] ->
      (* sortie vide → on renverse entree *)
      match List.rev f.entree with
      | [] -> failwith "File vide"
      | t :: q -> (t, { entree = []; sortie = q })

let f0 = file_vide;;
let f1 = enfiler 1 f0;;
let f2 = enfiler 2 f1;;
let f3 = enfiler 3 f2;;

let (x1, f4) = defiler f3;;  (* x1 = 1 *)
let (x2, f5) = defiler f4;;  (* x2 = 2 *)

let f6 = enfiler 4 f5;;
let (x3, f7) = defiler f6;;  (* x3 = 3 *)
let (x4, f8) = defiler f7;;  (* x4 = 4 *)

est_vide f8;;  (* true *)


(*MUTABLE (comme en C)*)

(*Liste chainée*)
(*Tous les élèments doivent etre du meme type donc on crée value qui est un type pouvant contenir plusieurs types*)
type value = Int of int | Str of string
     
(*On peut modifier directement une valeur avec mutable f.entree <- x :: f.entree; *)
type 'a cellule = {
  mutable valeur : 'a;
  mutable suivant : 'a cellule option;
}


let ajout (liste1 : 'a cellule) (e : 'a) : 'a cellule =
  let c1 = { valeur = e; suivant = Some liste1} in 
  c1;;

let c2 : value cellule = {valeur = Int 5; suivant = None} in
let c3 : value cellule = {valeur = Int 6; suivant = Some(c2)} in
let c4 : value cellule= {valeur = Int 7; suivant = Some(c3)}  in
let c5 : value cellule= {valeur = Int 7; suivant = None}  in

c5.suivant <- c4;;
ajout c3 (Str "4");;

(*cellule 10 None*)   (* ❌ Impossible *)

(*Pile*)
type 'a stack = {
  mutable tete : 'a cellule option ;
  mutable taille : int;
}

let create_stack( ) : 'a stack =
  let pile = {tete = None; taille = 0} in 
  pile

let stack_push (e : 'a) (pile : 'a stack) : unit = 
  let c1 = {valeur = e; suivant = pile.tete} in
  pile.tete <- Some(c1);
  pile.taille <- pile.taille + 1
                 

let stack_top (pile : 'a stack) : 'a =
  match pile.tete with
  | None -> failwith "Pile vide"
  | Some c -> c.valeur
                
let stack_vide (pile : 'a stack) : bool = 
  pile.taille = 0
  
let stack_pop (pile : 'a stack) : unit =
  match pile.tete with
  | None -> failwith "Pile vide"
  | Some c ->
      pile.tete <- c.suivant;  (* le premier élément de la pile pointe vers le deuxieme element de la pile *)
      pile.taille <- pile.taille -1
                                   
               
let piler = create_stack() in
stack_push 3 piler;
stack_push 4 piler ;
stack_push 5 piler;
Printf.printf "TOP : %d\n" (stack_top piler) ;
stack_pop piler;
Printf.printf "TOP : %d\n" (stack_top piler) ;
stack_pop piler;                       
Printf.printf "TOP : %d\n" (stack_top piler) ;
stack_pop piler;                   
Printf.printf "TOP : %d\n" (stack_top piler) ;
stack_pop piler;                   


(*File*)

(*Version avec 2 piles (Pas des listes !)*)

(*version avec liste chainée*)
type 'a file = {
  mutable taille : int;
  mutable fin : 'a cellule option;
  mutable tete : 'a cellule option;

}

let create_file () : 'a file =
  { taille = 0; tete = None; fin = None }
  
let enfiler (e : 'a) (file : 'a file) : unit =
  let c = {valeur = e; suivant = None} in
  match file.fin with
  | None ->
      file.tete <- Some c;
      file.fin <- Some c
  | Some f ->
      f.suivant <- Some c;
      file.fin <- Some c;
      file.taille <- file.taille + 1
  
let file_top (file : 'a file) : 'a =
  match file.tete with
  |None -> failwith "file vide"
  |Some c -> c.valeur
               


let defiler (file : 'a file) : unit = 
  match file.tete with 
  |None -> failwith "file vide"
  |Some c -> 
      file.tete <- c.suivant;
      file.taille <- file.taille - 1;
      if file.tete = None then file.fin <- None
  
;;

let filer = create_file();;
enfiler 4 filer;;
enfiler 5 filer;;
enfiler 6 filer;;

defiler filer ;;
Printf.printf "%d" (file_top filer);;



(*Table de hachage*)
(* 1. Création d'une table avec une taille initiale suggérée de 10 *)
let ma_table = Hashtbl.create 10

(* 2. Ajouter des éléments (Clé -> Valeur) *)

Hashtbl.add ma_table "Ulysse" 20;
Hashtbl.add ma_table "Pénélope" 25

(* 3. Rechercher une valeur *)
let age = Hashtbl.find ma_table "Ulysse" (* Retourne 20 ou lève Not_found *)

(* 4. Version sécurisée (retourne une Option) *)
let age_opt = Hashtbl.find_opt ma_table "Inconnu" (* Retourne None *)

let age_bool = Hashtbl.mem ma_table "Inconnu" (*renvoie un booléen*)


(*Dictionnaire en ocaml*)
type ('a,'b) dict =
{
mutable data: 'a * 'b list;
mutable size: int
} ;;







