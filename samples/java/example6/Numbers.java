//SOLUTION
//OUT evens.txt odds.txt
import java.io.*;
import java.util.*;

public class Numbers
{
   public static void main(String[] args) throws IOException
   {
      Scanner in = new Scanner(new File("values.txt"));
      PrintWriter odds = new PrintWriter("odds.txt");
      PrintWriter evens = new PrintWriter("evens.txt");
      //HIDE
      while (in.hasNextInt())
      {
         int n = in.nextInt();
         if (n % 2 == 0) evens.println(n); else odds.println(n);
      }
      evens.close();
      odds.close();
      //EDIT
   }
}
