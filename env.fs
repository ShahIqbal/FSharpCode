(*let last l =
  let rec hmm c b =
    match c with
      |[] -> 0
      |x::xs -> hmm xs b+1
  hmm l 0

last []

let reverse l =
  let rec rev c b =
    match b with
      |[] -> c
      |x::xs -> rev (x::c) xs
  rev [] l

let palindrome l =
  if reverse l = l then true else
    false

palindrome [1;2;3;2]



let rec secondLast l =
  match l with
    |[] -> 0
    |x::xs -> if xs.Length = 1 then x else secondLast xs

secondLast[2;3;4;5;6;7]


let rec hello l =
  match l with
    | [] -> []
    | x::xs -> [x;x] @ hello xs

hello [1;2;3;4]

*)



(* VIP *) (*
let rec inter item list =
  match list with
    |[] -> [[item]]
    |x::xs -> (item::list)::(List.map(fun d -> x::d)) (inter item xs)

inter 0 [1;2]


let rec perm l =
  match l with
    | [] -> [[]]
    | x::xs ->  (List.map(fun f -> x::f))(perm xs)

perm [1;2;3]

*)

let func l = List.map(fun f -> l ) l

func [1;2;3;4]
