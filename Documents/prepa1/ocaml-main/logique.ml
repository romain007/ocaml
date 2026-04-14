type expr =
  | Var of string
  | Const of bool
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Imp de expr * expr (* Implication *)

(* 1. Fonction de simplification (La clé de l'algo) *)
let rec simplifier e = match e with
  | Not (Const b) -> Const (not b)
  | And (Const true, e) | And (e, Const true) -> simplifier e
  | And (Const false, _) | And (_, Const false) -> Const false
  | Or (Const true, _) | Or (_, Const true) -> Const true
  | Or (Const false, e) | Or (e, Const false) -> simplifier e
  | Imp (Const false, _) -> Const true  (* 0 -> P est toujours vrai *)
  | Imp (Const true, e) -> simplifier e
  | Imp (e, Const false) -> simplifier (Not e)
  | _ -> e

(* 2. Fonction de substitution : remplace une variable par une constante *)
let rec substitue var b e = match e with
  | Var v when v = var -> Const b
  | Var v -> Var v
  | Not e1 -> Not (substitue var b e1)
  | And (e1, e2) -> And (substitue var b e1, substitue var b e2)
  | Or (e1, e2) -> Or (substitue var b e1, substitue var b e2)
  | Imp (e1, e2) -> Imp (substitue var b e1, substitue var b e2)
  | Const _ as c -> c

(* 3. L'algorithme de Quine pour tester si c'est une TAUTOLOGIE *)
let rec est_tautologie e =
  match simplifier e with
  | Const b -> b  (* On est en bas ! On remonte la valeur *)
  | e_simplifie ->
      (* On choisit la première variable qu'on trouve *)
      let v = choisir_variable e_simplifie in 
      (* On vérifie que c'est vrai pour v=true ET pour v=false *)
      (est_tautologie (substitue v true e_simplifie)) 
      && 
      (est_tautologie (substitue v false e_simplifie))