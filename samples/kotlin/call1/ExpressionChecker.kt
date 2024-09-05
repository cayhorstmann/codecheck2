//CALL "(3+4)*5"
//CALL "(3+(4-5)*6)-7"
//CALL "(4+3+(4-5)*6)-7)"
//CALL "((3+(4-5)*6)-7"
//CALL "4+(3+(4-5)*6)-7"
fun checkParentheses(expression: String): Boolean
{
    val stk = ArrayDeque<String>()
    for (i in 0..(expression.length-1))
    {
        val part = expression.substring(i, i + 1)
        stk.addFirst(part)
        //HIDE
        if (part == ")")
        {
            var done = false
            while (!done)
            {
                if (stk.isEmpty())
                    return false
                val top = stk.removeFirst()
                done = (top == "(")
            }
        }
        //EDIT
    }

    while (!stk.isEmpty())
    {
        val part = stk.removeFirst()
        //HIDE
        if (part == "(")
        {
            return false
        }
        //EDIT
    }

    //HIDE
    return true
    //EDIT return . . .
}
