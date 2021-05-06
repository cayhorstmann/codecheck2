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
   void print(bool x) { cout << (x ? "true" : "false"); }
   void print(string x) {
      cout << "\"";
      for (size_t i = 0; i < x.length(); i++) {
         if (x[i] == '\0') cout << "\\0";
         else if (x[i] == '\n') cout << "\\n";
         else if (0 < x[i] && x[i] < ' ')
            cout << "\\x" << hex << setw(2) << setfill('0') << (int) x[i] << dec;
         else if (x[i] == '\\' || x[i] == '"') cout << "\\" << x[i];
         else cout << x[i];
      }
      cout << "\"";
   }
   void print(const char* x) {
      cout << "\"";
      for (size_t i = 0; x[i] != '\0'; i++) {
         if (x[i] == '\n') cout << "\\n";
         else if (0 < x[i] && x[i] < ' ')
            cout << "\\x" << hex << setw(2) << setfill('0') << (int) x[i] << dec;
         else if (x[i] == '\\' || x[i] == '"') cout << "\\" << x[i];
         else cout << x[i];
      }
      cout << "\"";
   }

   bool eq(int x, int y) { return x == y; }
   bool eq(char x, char y) { return x == y; }
   bool eq(double x, double y) { return fabs(x - y) < 1E-12; }
   bool eq(float x, float y) { return fabs(x - y) < 1E-12; }
   bool eq(bool x, bool y) { return x == y; }
   bool eq(string x, string y) { return x == y; }
   bool eq(const char* x, const char* y) { return strcmp(x, y) == 0; }
}
