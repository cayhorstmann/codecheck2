//SOLUTION
//ARGS values.txt
//ARGS values2.txt

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Numbers
{
   public static void main(String[] args) throws FileNotFoundException
   {
      Scanner in = new Scanner(new File(args[0]));      
      while (in.hasNextInt())
      {
         int n = in.nextInt();
         System.out.println(n * n);
      }
   }
}
