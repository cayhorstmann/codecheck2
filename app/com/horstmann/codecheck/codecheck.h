#include <iostream>
#include <iomanip>
#include <vector>
#include <string>

namespace codecheck {

   void print(int x);
   void print(char x);
   void print(double x);
   void print(float x);
   void print(bool x);
   void print(std::string x);
   void print(const char* x);

   bool eq(int x, int y);
   bool eq(char x, char y);
   bool eq(double x, double y);
   bool eq(float x, float y);
   bool eq(bool x, bool y);
   bool eq(std::string x, std::string y);
   bool eq(const char* x, const char* y);

   template<typename T> void print(std::vector<T> xs) { 
      std::cout << "{"; 
      for (int i = 0; i < xs.size(); i++) {
         if (i > 0) { std::cout << ","; } std::cout << " ";
         print(xs[i]);
      }
      std::cout << " }";
   }

   template<typename T> bool eq(std::vector<T> xs, std::vector<T> ys) { 
      if (xs.size() != ys.size()) return false;
      for (int i = 0; i < xs.size(); i++) {
         if (!eq(xs[i], ys[i])) return false;
      }
      return true;
   }

   template<typename T> void compare(T x, T y) { 
      print(x); std::cout << std::endl;
      print(y); std::cout << std::endl;
      std::cout << std::boolalpha << eq(x, y) << std::endl;
   }
}
