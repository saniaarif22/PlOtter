![Alt Img](https://github.com/saniaarif22/PlOtter/blob/master/Plotter_F-03.png?raw=true)

### Requirements ###
	OCaml
	C++11
    
#### Sample Output ###

![Alt text](https://github.com/saniaarif22/PlOtter/blob/master/sample/sample1.png?raw=true "Sample 1")

### Steps to Run: ###

  - Navigate to the src/ folder
  > cd src
  
  - Run the make file
  > make
  
  - Run the program, <program name>.svg is created
  > ./plt /path_to_your_program.plt

  - Open the svg (hello.svg) file in a browser to view


#### If you intentionally want to do it the long way: ####
  
  - Write a program in plotter and save it as <program_name>.plt

  - Compile the plt program to generate a C++ program
  > ./plotter < <program_name>.plt > <program_name>.cpp

  - Compile the C++ program
  > g++ -std=c++11 prg_name.cpp
  
  - Run the executable which then generates a SVG 'hello.svg' as output
  > ./a.out

  - Open the svg file (hello.svg) in a browser to view


Logo Designed by Sriram Govindasamy (@Sriram3056)
