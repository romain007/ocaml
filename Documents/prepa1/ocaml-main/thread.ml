(*Module Thread*)
Thread.create fonction entre -> p
Thread.join p (*Met en pause le thread*)

(* Déclaration d'un type structuré permettant le stockage des arguments
2
(nb) et de la valeur de retour (res) de la fonction au_carre. *)
type args =
{
nb: int ;
mutable res : int ;
}

let au_carre (args: args): unit =

(* On écrit le résultat du calcul dans de la mémoire accessible par la
11
fonction appelante. *)
args.res <- args.nb * args.nb

let () =
let arg1 = {nb = 2; res = -1} in
let arg2 = {nb = 9; res = -1} in

(* On "lance" le thread pb : il doit exécuter la fonction au_carre sur

l'argument 2 et écrire le résultat dans le champs res de arg1. *)
let pa = Thread.create au_carre arg1 in


(* On "lance" le thread pb : il doit exécuter la fonction au_carre sur
22
l'argument 9 et écrire le résultat dans le champs res de arg2. *)
23 let pb = Thread.create au_carre arg2 in
24
25
(* On attend que les deux exécutions soient terminées. *)
26 Thread.join pa;
27 Thread.join pb;
28 assert (arg1.res = 4 && arg2.res = 81)