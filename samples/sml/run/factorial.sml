(* Description *)
(*HIDE*)
fun fact 0 = 1
  | fact n = n * fact(n - 1)
(*EDIT fun fact ... *)

fun main() =
  print (PolyML.makestring (map fact [0, 1, 2, 6, 20]));
