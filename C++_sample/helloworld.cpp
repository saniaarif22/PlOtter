#include <iostream>
#include <fstream>

using namespace std;

ofstream f;

// Prolog for the SVG image

void header() {
  string header = "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"> width=\"100\" height=\"30\"";
  f << header;
  f << "\n";
}

// SVG content
void put_in_svg(std::string content) 
{
  f << content;
  f << "\n";
}

// Read input and generate SVG image

int main() {
  f.open ("myFile.svg");
  string content = "<rect x=\"20\" y=\"20\" width=\"50\" height=\"20\" style=\"stroke: none; fill: red;\" />";

  header();
  put_in_svg(content);
  // Epilog for the SVG image
  f << "</svg>";
  f << "\n";

  f.close();

  return 0;
}