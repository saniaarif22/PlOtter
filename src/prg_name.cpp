#include <iostream>
#include <fstream>
using namespace std;
ofstream f;
// SVG content
void put_in_svg(std::string content)
{
  f << "<text x='250' y='150'>\n";
  f << content;
  f << "\n</text>\n";
}
// Read input and generate SVG image
int main() {
  f.open ("hello.svg");
  // Prolog for the SVG image
  f << "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"1024\" height=\"768\">"; 
  f << "\n"; 
put_in_svg( "Yo mama so fat");
  f << "</svg>"; 
return 0;
}
