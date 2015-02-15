#include <iostream>
#include <cmath>
#include <iomanip>
#include <vector>
#include <string>
#include <cstring>
#include "codecheck.h"

using namespace std;

namespace codecheck {
   void print(int x) { cout << x; }
   void print(char x) { cout << x; }
   void print(double x) { cout << x; }
   void print(float x) { cout << x; }
   void print(bool x) { cout << x; }
   void print(string x) { cout << x; }
   void print(const char* x) { cout << x; }

   bool eq(int x, int y) { return x == y; }
   bool eq(char x, char y) { return x == y; }
   bool eq(double x, double y) { return fabs(x - y) < 1E12; }
   bool eq(float x, float y) { return fabs(x - y) < 1E12; }
   bool eq(bool x, bool y) { return x == y; }
   bool eq(string x, string y) { return x == y; }
   bool eq(const char* x, const char* y) { return strcmp(x, y) == 0; }
}
