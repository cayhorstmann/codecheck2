/**
 This method accepts and integer array as a parameter, and then
 returns the "middle" value of the list.
 For an array of odd length, this would be the actual middle value.
 For an array of even length, there are TWO middle values, so only
 the first of the two values is returned.
 @param values, a list of integers
 @return, the "middle" element of the list
 */
//CALL listOf(85, 53, 973, 789, 56, 113, 712)
//CALL listOf(8, 7, 6, 5, 4, 3, 2, 1)
//CALL listOf(-2)
//CALL listOf(2, -5, 11, -21, -8, 7, -6)
//CALL listOf(43, 21)
fun middleOfList(values: List<Int>): Int
{
    //HIDE
    if (values.size % 2 != 0)
    {
        return values[values.size/2]
    }
    else
    {
        return values[(values.size/2) - 1]
    }
    //EDIT
}
