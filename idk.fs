type term = float * int
type poly = term list

exception EmptyList

(* Multiply a term by a polynomial. *)
let mtp(t:term,p:poly):poly =
  let c = fst t
  let d = snd t
  let gravity = List.map (fun (x, nm) -> x * c) p
  let gravity2 = List.map (fun (x, nm) -> nm + d) p

  (List.zip gravity gravity2)

let rec atp(t:term, p:poly):poly =
  match t, p with
    | t, [] -> [t]
  //| (_,_) , p -> p
    | (c, d) , (x, y) :: xs ->
      if d < y then (x, y) :: atp (t, xs)
      elif d = y then (x + c, y) :: xs
      else  (c, d) :: p


atp ((1.5, 3), [(2.5, 4); (1.0, 2); (1.5, 1)])



(*
//this is my atp matching before
if d < y then (x, y) :: atp (t, xs)
elif d = y then (x + c, y) :: atp(t, xs)
else  (c, d) :: p
*)




let rec addpoly p1 p2 =
  match (p1, p2) with
    | [], p2 -> p2
    | p1, [] -> p1
    | (a1, n1)::p1s, (a2, n2)::p2s ->
            if   n1 < n2 then (a2,    n2) :: addpoly p1  p2s
            elif n1 > n2 then (a1,    n1) :: addpoly p1s p2
            else              (a1+a2, n1) :: addpoly p1s p2s

let rec diff (p:poly):poly = 
  match p with
    | [] -> []
    | (x,y) :: xs ->
      if y = 0 then []
      else (x*float(y), y-1) :: diff (xs)

diff [(1.5, 3); (2.4, 2); (1.4, 1); (1.4, 0)]

(*
let rec atp t p2 =
  match (t, p2) with
    //| (), p2 -> p2
    | t, [] -> [t]
    | (a1, n1), (a2, n2)::p2s ->
        if   n1 < n2 then (a2,    n2) :: atp t p2s
        elif n1 > n2 then (a1,    n1) :: p2
        else              (a1+a2, n1) :: atp t p2s

*)
