use "factorial.sml";

fun main() =
  let
    val actual = map fact [0, 1, 2, 6, 20]
  in
    print ((PolyML.makestring (actual)) ^ "\nExpected: [1, 1, 2, 720, 2432902008176640000]\n")
  end;
