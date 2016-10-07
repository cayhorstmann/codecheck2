//SOLUTION
import java.util.Scanner;
public class Numbers
{
   public static void main(String[] args)
   {
      //HIDE
      Scanner in = new Scanner(System.in);
      boolean done = false;
      while (!done)
      {
         //SHOW . . .
         System.out.println("Enter a number, 0 to quit");
         //HIDE
         int n = in.nextInt();
         if (n == 0) done = true;
         else
         {
             int square =  n * n;
             //SHOW . . .
             System.out.println("The square is " + square);
             //HIDE
         }
      }
      //SHOW
   }
}
