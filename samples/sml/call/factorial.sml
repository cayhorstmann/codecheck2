(* Description *)
(*HIDE*)
(*CALL 3 *)
(*CALL 0 *)
fun fact 0 = 1
  | fact n = n * fact(n - 1)
(*EDIT fun fact ... *)
