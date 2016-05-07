import sys
import os

#If user gives a specific set of files from command line
if len(sys.argv)>1:
	testFiles = sys.argv[1:]
"""
#Make the program
os.system('make clean')
if(os.system('make')!=0):
	print "**** Make Failed"
	exit()
"""
#Get all the files in the tests dir
testFiles = os.listdir('./tests/')
testFiles = [ x for x in testFiles if x[-3:]=='plt' ]

#passing and failing
passed = []
failed = []

#Proceed if make succeeds
for file in testFiles:
	#For each test file perform the test. And print pass or failure
	runStr = './plt tests/' + file + ' 2> temp.out'
	#print 'Running for file : '+file +'\n'
	os.system(runStr)
	f = open('temp.out')
	s = f.readlines()
	f.close()
	if len(s)>0 and file[:4]=='pass':
		failed.append('**** FAILED for file '+file+'\n' + ' '.join(s) )
	else:
		passed.append( 'PASSED for --  '+file )

#Printing the results
print '---------- PASSED TESTS ------------'
for i in passed:
	print i
print '---------- FAILED TESTS ------------'
for i in failed:
	print i

print '---------- TESTS STATS------------'
print 'Passed : ' + str(len(passed))
print 'Failed : ' + str(len(failed))
