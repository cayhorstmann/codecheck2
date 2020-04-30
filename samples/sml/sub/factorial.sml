(* Description *)
(*HIDE*)
fun fact 0 = 1
  | fact n = n * fact(n - 1)
(*EDIT fun fact ... *)

fun main() =
  let
    val arg = 3; (*SUB 0; 1; 6; 20 *)
    val actual = fact arg
  in
    print (PolyML.makestring (actual))
  end;
