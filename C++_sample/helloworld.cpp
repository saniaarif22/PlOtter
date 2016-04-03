#include <iostream>
#include <fstream>

using namespace std;

ofstream f;

// SVG content
void put_in_svg(std::string content) 
{
  f << "<text x=\"250\" y=\"150\">\n";
  f << content;
  f << "\n</text>\n";
}

// Read input and generate SVG image

int main() {
  f.open ("hello.svg");
  
  // Prolog for the SVG image
  f << "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"> width=\"100\" height=\"30\"";
  f << "\n";
  
  string content = "Hello world";
  put_in_svg(content);
  
  // Epilog for the SVG image
  f << "</svg>";
  f << "\n";

  f.close();

  return 0;
}