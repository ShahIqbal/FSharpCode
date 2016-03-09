type term = float * int
type poly = term list

exception EmptyList
(*
let data2 = [(1.5, 8); (2.5, 4); (1.0, 2)]: poly
let data = (1.5, 3)

let c = fst data

let d = snd data

let gravity = List.map (fun (x, nm) -> x * c) data2
let gravity2 = List.map (fun (x, nm) -> nm + d) data2

let hagu = (List.zip gravity gravity2) :poly
*)

//let zs = List.init 5 (fun i -> 2 * i + 1)

//let p = [(1.5,3); (2.0,1)]
//let havoc = List.map ( fun (x, y) -> yx, y - 1 ) p


//let gravity2 list2 = List.map(fun x -> x + d) list2
let rec multpolys(p1:poly,p2:poly) =
  //let rec helper pp1 pp2 =

    match p1, p2 with
      | [] , p2 -> p2
      | p1 , [] -> p1
      | (x, y) :: x1s , (c, d):: x2s ->
             [(x*c, y+d)] @ multpolys(p1, x2s) @ multpolys (x1s, p2)

  //let c = List.split helper p1 p2


multpolys ([(3.0, 2); (2.0,1)] , [(1.0, 4); (4.0, 1)])
