//SOLUTION
//IN 3\n-3\n0\n
using System;
public class Runner
{
   public static void Main()
   {
      //HIDE
      bool done = false;
      while (!done)
      {
         //SHOW . . .
         Console.WriteLine("Enter a number, 0 to quit");
         int n = Convert.ToInt32(Console.ReadLine());
         //HIDE
         if (n == 0) done = true;
         else
         {
             int square =  n * n;
             //SHOW . . .
             Console.WriteLine("The square is " + square);
             //HIDE
         }
      }
      //SHOW
   }
}
