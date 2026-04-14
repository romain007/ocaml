
let carre y = y * y  (* Définition globale de la fonction carre *)

let puissance4 x = carre (carre x)  (* Utilisation directe de carre *)
                                    
let y = 2 in y+3;; (* Cela signifie : on utilise l'expression y = 2 dans y + 3 *)
                                    
let puissance4 x =
  let cube y = y * y * y in (* Définition locale *)
  cube(cube x)       (* Utilisation *)

                     
let syracuse x = 
  if x mod 2 = 0 then x/2
  else x*3 +1;;

let rec syracuse2 (n,k) = (* Fonction récursive *)
  if k = 0 then n 
  else if n mod 2 = 0 then syracuse2(n/2,k-1)
  else
    syracuse2(n * 3 +1,k-1);;
    
  
let max (x,y) =
  if x > y then x
    
  else y;;
  
let abs x =
  if x < 0. then -.x
  else
    x;;
       

let rec fact n = 
  if n = 0 then
    1
  else
    n * fact(n-1);;

      
let expo (a,n) = 
  a ** n;;

  
let rec expo2 (a,n) = 
  if n = 1 then a
  else
    a*expo2(a,n-1);;

      
  
let racine(a,b,c) = 
  let delta = b*b - 4*a*c in
  
  if delta > 0 then 2
  else if delta = 0 then 1 
  else 0;;

    
let f2 (x,y) = 
  if (x-y) mod 2 = 0 then x + y
  else
    x - y;;

    
let valeur1 = 3;;
let valeur2 = 4;;

type complexe = { (* On définit le type complexe (nouvel objet) avec pour chaque variables le type attendu *)
  re: int;
  im: int;
}
                   
let nombre = {re = 2; im = -5}  (* On crée un objet complexe. On utilise pas le mot complexe mais directement ses arguments *)     
let nombre3 = {re = 4; im = -5}
let nombre2 = {nombre with re = 4} (* On recopie un autre objet en changeant certaines valeurs avec with*)

let fonction_c nombre =   (*conjugué nombre complexe*)
  {nombre with im = -nombre.im}
  
let produit_c (nombre1,nombre2) = 
  {re = nombre1.re*nombre2.re - nombre1.im*nombre2.im ;im = nombre1.re*nombre2.im + nombre1.im * nombre2.re}

type piece = Pion | Cavalier | Fou | Tour | Roi | Dame (* Crée un type avec des constructeurs, il ne peut prendre que des valeurs faisant partie des constructeurs*)

type chifoumi = Pierre | Feuille | Ciseau
type resultat = Defaite | Nul | Victoire

let choix1 = Pierre (* Pierre est définis dans piece. Il a donc le type constructeur Pierre*)
let choix2 = Feuille
let choix3 = Ciseau

let resultat2 choix =
  match choix with   (* Permet d'énumérer les possibilités. Si choix est de type Pierre, on fait ça*)
  | Pierre -> "Vous avez choisi Pierre"
  | Feuille -> "Vous avez choisi Feuille"
  | Ciseau -> "Vous avez choisi Ciseau"

let chifoumi2 (joueur1,joueur2) =  (*Enumère les possibilités. | Permet d'ajouter plusieur possibilité pour une meme expression. La fleche signifie return*)
  match (joueur1,joueur2) with
  | (Pierre,Ciseau) | (Feuille,Pierre) | (Ciseau,Feuille) -> (Victoire,Defaite)
  | (Pierre,Pierre) | (Feuille,Feuille) | (Ciseau,Ciseau) -> (Nul,Nul)
  |(Ciseau,Pierre) | (Pierre,Feuille) | (Feuille,Ciseau) -> (Defaite,Victoire)
                                                            
                                                            
  
type valeur = Roi | Dame | Valet | Point of int  (*Le constructeur Point possède une valeur associé de type int*)
type couleur = Trefle | Pique | Coeur | Carreau
type carte = Joker | Carte of valeur * couleur
let c = Point 5   (* c est de type constructeur Point avec la valeur associé 5*)

let dix_de_pique = Carte (Point 10, Pique)  (* La carte à un type constructeur Carte qui est associé à un type constructeur valeur et couleur. Le type constructeur valeur est construit avec Point et sa valeur 10 et couleur avec Pique*)


type intlist = Vide | Cell of int * intlist ;; (* Crée une liste de la forme ma_liste. Chaque cellule de la liste possède un entier et une autre cellule. Type récursif*)

let ma_liste = Cell (1, Cell (2, Cell (3, Vide)))
    
let rec appartient element liste =  (* On regarde si l'élèment appartient à la liste. Fonction récursive pour le booléen, regarde chaque élèment de la liste. Si trouver True et le OR s'arrète, sinon on arrive à la fin et False*)
  match liste with
  | Vide -> false
  | Cell(i,s) -> i == element || appartient element s
;;

let ma_liste = 1 :: [2; 3; 4];; (* L'élèment 1 est ajouté en tete de la liste [2;3;4] (.append)*)
let liste4 = [5;5;5;5;5]
             
(*List.hd l  et
  List.tl l renvoient respectivement la tˆete et la suite d’une liste l
  
  List.length l renvoie la longueur de la liste l
  
   l1 @ l2 renvoie la concat́enation de l1 et l2.*)

let rec longueur liste3 = 
  if liste3 = [] then 0
  else
    (longueur (List.tl liste3)) + 1
;;
  
let rec concat (liste1,liste2) = 
  if liste1 = [] then liste2 
  else 
    concat ((List.tl liste1),((List.hd liste1) :: liste2)) ;;

let rec max liste= 
  if liste = [] then 0
  else if List.hd liste > (max (List.tl liste))
  then List.hd liste 
  else
    0
  
let rec somme liste = 
  if liste = [] then 0
  else 
    List.hd liste + somme( List.tl liste)

let rec produit liste = 
  if liste = [] then 1
  else 
    List.hd liste * produit( List.tl liste)

let rec syracuse3 (a,n) = (* Fonction récursive *)
  if n = 0 then [] 
  else if a mod 2 = 0 then a :: syracuse3(a/2,n-1)
  else
    a :: syracuse3(a * 3 +1,n-1);;

let liste_m = [[1;2;3;4;5];[5;4;5;4];[3;3;4]]
              
let rec liste_concat liste = (* Concatène toutes les listes *)
  if liste = [] then []
  else
    concat((List.hd liste),liste_concat(List.tl liste))
  
let fonction x =
  x*x
  
let rec liste_fonction (liste,fonction) = 
  if liste = [] then []
  else
    fonction(List.hd liste) :: liste_fonction((List.tl liste),fonction)

      (* — %d pour les entiers
         — %f pour les flottants
         — %c pour les caractères
         — %s pour les chaines de caractères *)

let sequence = "World";;
Printf.printf "Hello %s!" sequence;;


(*if condition then
   begin
     instruction_1 ;
     instruction_2
   end
*)

(*while condition1 do
instructions
done;;*)

for k = 1 to 10 do 
  print_int k
done;;

let divise n =
  for k = 1 to n do
    if n mod k = 0 then Printf.printf "%d est un diviseur \n" k
        
  done;;

      let liste = [|[|4;4;|];[|5;5|]|]
let liste2 = [|3;100000;3;2;23;12;433;2|]
let fonction liste n =
  let x = ref false in
  for i=0 to n-1 do
    for j=0 to n-1 do
      if liste.(i).(j) mod 2 = 0 then x := true
                                    
    done;
  done;
  !x;;
                                      

type complexe = {mutable re : float ; mutable im : float}
let z = {re = 1. ; im = 1.} 
        
        
let conjugue x =
  z.re <- z.re +. 1.;;

let x = ref 5 ;;
x ;;
!x ;;
x := !x + 1 ;;
!x ;;                

(*
Array.length qui prend en entrée un tableau et renvoie sa longueur ;
Array.make n v qui permet de créer un tableau à n elèment valant initia-
lement tous la valeur v 
Array.init n f qui permet de créer un tableau à n éleèment tel que
l’élèment d’indice i vaut f(i)
*)

let maximum t =
  let w = ref t.(0) in
  for i=1 to Array.length(t)-1 do
    if t.(i) > !w then w := t.(i)
  done;
  !w;;


let swap t i j = 
  let save = t.(i) in
  
  
      
  t.(i) = t.(j); 
  t.(j) = save;

let somme t = 
  let var = ref 0 in 
  for i=0 to Array.length(t)-1 do
    var := !var + t.(i)
  done;
  var;;
               
let trie t = 
  let boo = ref true in
  for i=0 to (Array.length(t)-2) do 
    if t.(i) > t.(i+1) then  boo := false
  done;
  !boo

    
let to_list t =
  let liste = ref [] in 
  for i=0 to (Array.length(t)-1) do
    liste := t.(i) :: !liste
  done;
  !liste;; 


(*let m1 = Array.make matrix n m v *)



























  
  
    