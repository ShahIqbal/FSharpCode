(* Question 2 *)

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



(* val mtp : t:term * p:poly -> poly *)

(* Add a term to a polynomial. *)
let rec atp(t:term,p:poly):poly =
  match t, p with
    | t, [] -> [t]

    | (c, d) , (x, y) :: xs ->
      if d < y then (x, y) :: atp (t, xs)
      elif d = y then (x + c, y) :: xs
      else  (c, d) :: p
(* val atp : t:term * p:poly -> poly *)

(* Add two polynomials.  The result must be properly represented. This means you
cannot have more than one term with the same exponent, you should not have a
term with a zero coefficient, except when the whole polynomial is zero and the
terms should be decreasing order of exponents.   Thus, for example,
5.2 x^7 - 3.8 x^4 +2.0 x - 1729.0 should be represented as
[(5.2,7);(-3.8,4);(2.0,1);(-1729.0,0)] *)

let rec addpolys(p1:poly,p2:poly):poly =
  match (p1, p2) with
    | [], p2 -> p2
    | p1, [] -> p1
    | (a1, n1)::x1s, (a2, n2)::x2s ->
          if   n1 < n2 then (a2, n2) :: addpolys (p1,  x2s)
          elif n1 > n2 then (a1, n1) :: addpolys (x1s, p2)
          else              (a1+a2, n1) :: addpolys (x1s, x2s)
(* val addpolys : p1:poly * p2:poly -> poly *)

(* Multiply two polynomials.  All the remarks above apply here too. Raise an
exception if one of the polynomials is the empty list. *)
let rec multpolys(p1:poly,p2:poly) = failwith "Error - not implemented"
(* val multpolys : p1:poly * p2:poly -> poly *)

(* This is the tail-recursive version of Russian peasant exponentiation.  I have
done it for you.  You will need it for the next question.  *)
let exp(b:float, e:int) =
  let rec helper(b:float, e:int, a: float) =
    if (b = 0.0) then 0.0
    elif (e = 0) then a
    elif (e % 2 = 1) then helper(b,e-1, b*a)
    else helper(b*b,e/2,a)
  helper(b,e,1.0)

(* Here is how you evaluate a term. *)
let evalterm (v:float) ((c,e):term) = if (e=0) then c else c * exp(v,e)

(* Evaluate a polynomial viewed as a function of the indeterminate.  Use the function
above and List.fold and List.map and a dynamically created function for a one-line
answer.  *)
let evalpoly(p:poly,v:float):float = //failwith "Error - not implemented"
(* val evalpoly : p:poly * v:float -> float *)

(* Compute the derivative of a polynomial as a symbolic representation.  Do NOT use
deriv defined above.  I want the answer to be a polynomial represented as a list.
I have done a couple of lines so you can see how to raise an exception.  *)

let rec diff (p:poly):poly =
  match p with
    | [] -> []
    | (x,y) :: xs ->
      if y = 0 then []
      else (x*float(y), y-1) :: diff (xs)

         // remove this case and write your code.
(*  val diff : p:poly -> poly *)
