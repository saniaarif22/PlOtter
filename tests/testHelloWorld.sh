#!/bin/bash
make
./helloWorld
Output="$(diff -B hello.svg test-hello.out)"
if [ -z $Output ]
then
echo "###### SUCCESS"
else
echo "###### FAILED"
fi
make clean
