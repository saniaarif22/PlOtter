import sys
import os

#Test files list
testFiles = ['hello','sum','vdecl']
#If user gives a specific set of files from command line
if len(sys.argv)>1:
	testFiles = sys.argv[1:]

#Make the program
os.system('make clean')
if(os.system('make')!=0):
	print "**** Make Failed"
	exit()

#Proceed if make succeeds
for file in testFiles:
	#For each test file perform the test. And print pass or failure
	runStr = './plotter < tests/test_'+file+'.plt > temp.cpp'
	if(os.system(runStr)!=0):
		print '**** ERROR : Cannor run '+file+' file with plotter'
		continue
	if(os.system('g++ temp.cpp')!=0):
		print '**** ERROR : cannot compile c++ code for file '+file+'.plt'
		continue
	os.system('./a.out')
	os.system('diff hello.svg tests/test_'+file+'.svg > temp.out')
	f = open('temp.out')
	s = f.readlines()
	f.close()
	if len(s)>0:
		print '**** FAILED for file '+file
	else:
		print 'PASSED for --  '+file
