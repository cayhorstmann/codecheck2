#include <iostream>
#include <iomanip>
#include <vector>
#include <string>

using namespace std;

namespace codecheck {

   void print(int x);
   void print(char x);
   void print(double x);
   void print(float x);
   void print(bool x);
   void print(string x);
   void print(const char* x);

   bool eq(int x, int y);
   bool eq(char x, char y);
   bool eq(double x, double y);
   bool eq(float x, float y);
   bool eq(bool x, bool y);
   bool eq(string x, string y);
   bool eq(const char* x, const char* y);

   template<typename T> void print(vector<T> xs) { 
      cout << "{"; 
      for (int i = 0; i < xs.size(); i++) {
         if (i > 0) { cout << ","; } cout << " ";
         print(xs[i]);
      }
      cout << " }";
   }

   template<typename T> bool eq(vector<T> xs, vector<T> ys) { 
      if (xs.size() != ys.size()) return false;
      for (int i = 0; i < xs.size(); i++) {
         if (!eq(xs[i], ys[i])) return false;
      }
      return true;
   }

   template<typename T> void compare(T x, T y) { 
      print(x); cout << endl;
      print(y); cout << endl;
      cout << boolalpha << eq(x, y) << endl;
   }
}
