//
// gensvg.cpp
//

#include <iostream>
#include <fstream>

using namespace std;

ofstream f;

// Prolog for the SVG image

void header() {
  std::string header = "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"> width=\"100\" height=\"30\"";
  if (f.is_open())
  {
    f << header;
    f << "\n";
  }
  else
  {
    f.open("myFile.svg");
    f << header;
    f << "\n";
  }
}

// Epilog for the SVG image

void footer() {
  std::string footer = "</svg>";
  if (f.is_open())
  {
    f << footer;
    f << "\n";
  }
  else
  {
    f.open("myFile.svg");
    f << footer;
    f << "\n";
  }
}

// Generation of the two segments


void bar() 
{
  std::string content = "<rect x=\"20\" y=\"20\" width=\"50\" height=\"20\" style=\"stroke: none; fill: red;\" />";
  if (f.is_open())
  {
    f << content;
    f << "\n";
  }
  else
  {
    f.open("myFile.svg");
    f << content;
    f << "\n";
  }
}

// Read input and generate SVG image

int main() {
  unsigned m, n;

  f.open ("myFile.svg");

  cin >> m;
  cin >> n;

  header();
  bar();
  footer();

  f.close();

  return 0;
}