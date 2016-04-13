# PlOtter

    The source files are located in src/
    
Steps to Run:

  - Navigate to the src/ folder
  > cd src
  
  - Run the make file
  > make

  - Write a program in plotter and save it as <program_name>.plt

  - Compile the plt program to generate a C++ program
  > ./plotter < <program_name>.plt > <program_name>.cpp

  - Compile the C++ program
  > g++ prg_name.cpp
  
  - Run the executable which then generates a SVG 'hello.svg' as output
  > ./a.out

  - Open the svg using a browser to view
