(* Question 3 *)

open System.Collections.Generic;;

type ListTree<'a> = Node of 'a * (ListTree<'a> list)

let bfIter f ltr =
    let todo = Queue<ListTree<'a>> ()
    todo.Enqueue ltr
    while (todo.Count <> 0) do
      let (Node (node, lst)) = todo.Dequeue()
      List.iter (todo.Enqueue) lst
      f node;;




(*   This is how you set up a new Queue: let todo = Queue<ListTree<'a>> () *)


let n5 = Node(5,[])
let n6 = Node(6,[])
let n7 = Node(7,[])
let n8 = Node(8,[])
let n9 = Node(9,[])
let n10 = Node(10,[])
let n12 = Node(12,[])
let n11 = Node(11,[n12])
let n2 = Node(2,[n5;n6;n7;n8])
let n3 = Node(3,[n9;n10])
let n4 = Node(4,[n11])
let n1 = Node(1,[n2;n3;n4])




bfIter (fun n -> printfn "%i" n) n1;;
