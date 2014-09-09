import java.util.Scanner;
public class Numbers
{
   public static void main(String[] args)
   {
      Scanner in = new Scanner(System.in);
      boolean done = false;
      while (!done)
      {
         System.out.println("Enter a number, 0 to quit");
         int n = in.nextInt();
         if (n == 0) done = true;
         else System.out.println("The square is " + n * n);
      }
   }
}
