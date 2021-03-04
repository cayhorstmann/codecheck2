#include <iostream>

using namespace std;

int main() 
{
   bool done = false;
   while (!done) 
   {
      cout << "Enter a number, 0 to quit: ";
      int n;
      cin >> n;
      if (n == 0) { done = true; }
      else { cout << "Square: " << n * n << endl; }
   }

   return 0;
}
