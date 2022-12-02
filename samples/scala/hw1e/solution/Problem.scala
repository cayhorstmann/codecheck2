//FORBIDDEN (^|\PL)for\s*\(
//Do not use "for"
//FORBIDDEN (^|\PL)while(\PL|$)
//Do not use "while"
//FORBIDDEN (^|\PL)var(\PL|$)
//Do not use "var"
//FORBIDDEN (^|\PL)def(\PL|$)
//Do not use "def"
object Problem {
  /**
    * Checks whether a sequence of integer values in increasing
    * @aparam a the sequence (of length >= 1)
    * @return true if a(i) < a(i+1) for all adjacent elements
    */
//CALL 1.to(100)
//CALL Vector(1, 4, 9, 16, 9, 36)
//CALL Vector(1, 4, 9, 16, 16, 36)
//CALL Vector(1)
   val isIncreasing = (a: Seq[Int]) => a.dropRight(1).zip(a.drop(1)).forall(p => p._1 < p._2)
}
