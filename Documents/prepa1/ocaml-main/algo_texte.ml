(*ALGO BOYER MOORE RECHERCHE TEXTE*)

let texte = 
  [| 'h'; 'e'; 'u'; 'r'; 'e'; 'u'; 'x'; ' '; 'q'; 'u'; 'i'; ' '; 
     'c'; 'o'; 'm'; 'm'; 'e'; ' '; 'u'; 'l'; 'y'; 's'; 's'; 'e' |]

let mot = [| 'u';'l';'y';'s';'s';'e' |]
  
let decalage mot = 
  let m = Array.length mot in
  let table = Hashtbl.create m in
  for i = 2 to m do
    if Hashtbl.mem table mot.(m-i) || mot.(m-i) = mot.(m-1) then 
      ()
    else 
      Hashtbl.add table mot.(m-i) (i-1)
  done;
  table;;



let boyer (mot, texte) = 
  let table = decalage mot in
  let l = Array.length(texte) in
  let m = Array.length(mot) in 
  let find = ref false in
  let i = ref (m-1) in
  let j = ref (m-1) in
  while (not !find) && (!i < l) do
    
    
    
    (*Si la fin du mot ne match pas*)
    if mot.(!j) != texte.(!i) then 
        
      let resultat = Hashtbl.find_opt table texte.(!i) in
      match resultat with
                         (*si cette lettre est dans le mot, on décale suivant la table de décalage*) 
      |Some decalage  -> begin 
          i := !i +(m - 1 - !j) + decalage; 
          j := (m-1) 
        end
          
            (*si cette lettre n'est pas dans le mot, on fais le grand décalage (m-1-!j) pour repartir de la fin*)
      |None -> begin
          i := !i +(m - 1 - !j) + m;
          j := (m -1)
        end
               
                    
    else 
      
    if !j = 0 then find := true
    else 
      begin 
        j := !j - 1;
        i := !i -1
      end
  done;
  !find;;


let frequencehuffman texte = 
  