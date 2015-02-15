#include <iomanip>
#include <string>
#include <sstream>

using namespace std;

int time_to_int(string time) {
   int h, m, s;
   char colon;
   stringstream(time) >> h >> colon >> m >> colon >> s;
   return h * 60 * 60 + m * 60 + s;
}

string int_to_time(int n) {
   stringstream s;
   s << setfill('0') << setw(2) << n / (60 * 60) << ":"
      << setw(2) << (n / 60) % 60 << ":" 
      << setw(2) << n % 60 << ends;
   return s.str();
}

//CALL "9:30:15", "0:45:45"
//CALL "22:30:15", "2:45:00"
string add_time(string from, string amount) {
   return int_to_time((time_to_int(from) + time_to_int(amount)) % (24 * 60 * 60));
}

