public class Numbers
{
   /**
      Counts the number of digits with value 7 in a given number
      @param n any non-negative number
      @return the number of digits with value 7 in the decimal representation of n
   */
   //CALL 7777
   //CALL 1729
   //CALL 35
   //CALL 7
   //CALL 0
   public int countSevens(int n)
   {
      int count = 0;
      //HIDE
      while (n > 0)      
      //EDIT while (...)
      {
         //HIDE
         if (n % 10 == 7) count++;
         n /= 10;
         //EDIT ...
      }
      return count;
   }
}
