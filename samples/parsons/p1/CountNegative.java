public class CountNegative
{
   /**
      Counts the negative elements in a given array
      @param a an array of integers
      @return the count of negative integers in a
   */
   //CALL new int[] { 1, 2, -3, -4, 5, 6 }
   //CALL new int[] { -1, -2, -3, -4, -5 }
   //CALL new int[] { -1, -2, 0, 4, -5 }
   //CALL new int[] { 1, 2, 3, 4 }
   //CALL new int[] { }
   public int countNegativeElements(int[] a) {
//TILE
      int count = 0;
      //OR int count = 1;
      int i = 0;
      //OR int i = 1;
      
      while (i < a.length)
      //OR while (i <= a.length)
      {
         if (a[i] < 0)
         //OR if (a[i] <= 0)
         {
            count++;
         }
         i++;
      }
      return count;
      //OR return i;
//FIXED
   }
}
//OR 2
//else
//{
//OR }
