#!/bin/bash
make
./plotter < "programs/$1" > prg_name.cpp
g++ prg_name.cpp
./a.out
Output="$(diff -B hello.svg test_hello.svg)"
if [ -z $Output ]
then
echo "###### SUCCESS"
else
echo "###### FAILED"
fi
make clean
