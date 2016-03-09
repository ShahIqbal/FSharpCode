type transaction = Withdraw of int| Deposit of int| Checkbalance

let make_protected_account (opening_balance : int , password: string) =
    let balance = ref opening_balance

    fun (s:string, t:transaction) ->
      match s, t with
        |g, Withdraw(m) -> if (password.Equals(g))
                            then
                              if(!balance > m) then
                                balance := !balance - m
                                printfn "The new balance is %i" !balance
                              else
                                printfn "Insufficient funds"
                            else
                              printfn "Incorrect password"
        |g, Deposit(m) -> if(password.Equals(g))
                          then
                            balance := !balance + m
                            printfn "The new balance is %i" !balance
                          else
                            printfn "Password Incorrect"
        |g, Checkbalance -> if(password.Equals(g)) then
                              printfn "Balance is %i" !balance
                            else
                              printfn "Password incorrect"




let harry = make_protected_account(5000, "avara kadabra")
harry("avara kadabra", Checkbalance)
harry("avara kadabra", Withdraw(50))
harry("avara kadabra", Deposit(100))
harry("avara kadabra", Checkbalance)
