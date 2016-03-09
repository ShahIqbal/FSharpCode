(* Datatype of bank account transactions.*)
type transaction = Withdraw of int | Deposit of int | Checkbalance

(* Bank account generator. *)
let make_account(opening_balance: int) =
    let balance = ref opening_balance
    fun (t: transaction) ->
      match t with
        | Withdraw(m) ->  if (!balance > m)
                          then
                            balance := !balance - m
                            printfn "Balance is %i" !balance
                          else
                            printfn "Insufficient funds."
        | Deposit(m) -> (balance := !balance + m; (printf "Balance is %i\n" !balance))
        | Checkbalance -> (printf "Balance is %i\n" !balance);;

let morgoth = make_account(1000)
morgoth(Checkbalance)
