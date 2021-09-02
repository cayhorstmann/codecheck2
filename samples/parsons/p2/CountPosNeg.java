/**
   Print the number of positive and negative inputs.
*/
//IN 1 2 -3 -4 5 6 Q
//IN -1 -2 -3 -4 -5 Q
//IN -1 -2 0 4 -5 Q
//IN 1 2 3 4 Q
//IN Q
import java.util.Scanner;

public class CountPosNeg
{
   public static void main(String[] args)
   {
//TILE
      int positive = 0;
      int negative = 0;
//FIXED
      Scanner in = new Scanner(System.in);
      while (in.hasNextInt())
      {
//TILE
         int input = in.nextInt();
         if (input < 0)
         //OR if (input <= 0)
         {
            negative++;
         }
         if (input > 0)
         //OR if (input >= 0)
         {
            positive++;
         }
//FIXED
      }
      //TILE 2
      System.out.println("Positive: " + positive);
      System.out.println("Negative: " + negative);
//FIXED
   }
}
