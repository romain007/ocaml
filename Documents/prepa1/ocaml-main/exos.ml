let rec power a b =
  if b = 0 then 1
  else a * power (a) (b - 1)
;;

let rec sommation n =
  let paire x = 
    if x mod 2 = 0 then true
    else false
  in
  if n = 0 then 0
  else 
    if paire n then 
      power n n + sommation n-1
    else
      -1 * power n n + sommation n-1
;;

let rec f n = 
  if n = 0 then 1
  else
    n - m(f(n-1))
and
let rec m n =
  if n = 0 then 0 
  else
    n - m(f(m(n-1)))