
#include <stdio.h>
#include <stdbool.h>

int main() 
{
   bool done = false;
   while (!done) 
   {
      printf("Enter a number, 0 to quit: ");
      int n;
      scanf("%d",&n);
      if (n == 0) { done = true; }
      else { printf("Square: %d\n", n * n); }
   }

   return 0;
}
