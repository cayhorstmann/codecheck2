import java.util.Scanner;

fun main(args: Array<String>) {
      //HIDE
      var done = false
      while (!done)
      {
         //SHOW . . .
         System.out.println("Enter a number, 0 to quit")
         //HIDE
         val n = readln().toInt()
         if (n == 0) done = true
         else
         {
             val square =  n * n
             //SHOW . . .
             System.out.println("The square is " + square)
             //HIDE
         }
      }
      //SHOW
}
