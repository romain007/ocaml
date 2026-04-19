type 'a regex =
  | Vide
  | Epsilon
  | Lettre of 'a
  | Concat of 'a regex * 'a regex
  | Union of 'a regex * 'a regex
  | Etoile of 'a regex

let est_vide expr = 
  match expr with
  | Vide -> true
  | _ -> false
    
    
type etat = int ;;
type lettre = int ;;
type mot = lettre list ;;
type afd =
  {
    m : int;
    n : int;
    init: etat;
    final: etat;
    term: bool array;
    delta: etat array array
  } ;;

let automate = 
  {
    n= 4; (* États 0, 1, 2 et l'état 3 *)
    m= 3; (* Alphabet 0, 1, 2 *)
    init = 0;
    term = [| false; false; false; true |];
    final = 3;
    delta = [|
      (* État 0 : Seul '0' mène à 1, les autres n'existent pas *)
      [| 1; -1; -1 |]; 
      (* État 1 : Seul '1' mène à 2 *)
      [| -1; 2; -1 |]; 
      (* État 2 : Seul '2' mène à 3 *)
      [| -1; -1; 3 |];
      (* État 3 : État final, aucune sortie *)
      [| -1; -1; -1 |]
    |];
  } ;;
    
let adj2 = [|
  [];
  [2];
  [4];
  [1;2];
  [];
  [3;4;6];
  [4;7];
  [];
|]
(*renvoie l'etat accessible avec en entrée un état est une lettre*)
let delta_etat aut etat lettre = 
  aut.delta.(etat).(lettre)

let rec delta_etat_etendue aut etat mot = 
  match mot with
  |t::s -> let new_etat = delta_etat aut etat t in
      if new_etat <> -1 then delta_etat_etendue aut new_etat s
      else
        -1
  |[] -> etat

let accepte aut mot = 
  if delta_etat_etendue aut aut.init mot <> -1 then true
  else false
    


let graphe aut = 
  let adj = Array.make aut.n [] in
  for i = 0 to aut.n-1 do 
    for j = 0 to aut.m-1 do
      if delta_etat aut i j <> -1 then adj.(i) <- ((delta_etat aut i j) :: adj.(i) )
    done;
  done;
  adj;;


let afficher_file_int f =
  print_string "Queue [ ";
  (* Queue.iter applique une fonction à chaque élément sans les retirer *)
  Queue.iter (fun x -> print_int x; print_string " ") f;
  print_endline "]"
    
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

let appartient x tab =
  let trouve = ref false in
  for i = 0 to (Array.length tab) - 1 do
    if tab.(i) = x then trouve := true
  done;
  !trouve
    
let accessible automate =
  let adj = graphe automate in
  let liste = parcours_largeur adj automate.init in
  let acces = Array.make (automate.n) false in
  
  for i = 0 to (automate.n)-1 do
    Printf.printf "%d" i;
    if List.mem i liste then acces.(i) <- true;
  done;
  acces;;
  
let coaccessible automate =
  let adj = graphe automate in
  let liste = parcours_largeur adj automate.final in
  let acces = Array.make (automate.n) false in
  
  for i = 0 to (automate.n)-1 do
    Printf.printf "%d" i;
    if List.mem i liste then acces.(i) <- true;
  done;
  acces;;
  
let est_emonde automate = 
  let liste1 = coaccessible automate in
  let liste2 = accessible automate in 
  let result = ref true in
  for i = 0 to (Array.length liste1)-1 do
    if not (liste1.(i) && liste2.(i)) then result := false
        
  done;
  !result;;



let emondage automate = 
  let liste1 = coaccessible automate in
  let liste2 = accessible automate in 
  let enlever = [] in
  for i = 0 to (Array.length liste1)-1 do
    if not (liste1.(i) && liste2.(i)) then i :: enlever
  done;
  let automate2 = {
    automate with
    m = 
    