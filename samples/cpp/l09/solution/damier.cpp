#include <iostream>

using namespace std;

int ligne(int n) { return (n - 1) / 8 + 1; }
int colonne(int n) { return (n - 1) % 8 + 1; }
int damier(int r, int c) { return (r - 1) * 8 + c; }

main() {
   cout << "Numéro (entre 1 et 64): ";
   int n;
   cin >> n;
   cout << n << endl; // Show input 
   int l = ligne(n);
   int c = colonne(n);
   cout << "(" << l << ", " << c << ")" << endl;
   cout << damier(c, l) << endl;
   cout << damier(9 - c, 9 - l) << endl;
}
