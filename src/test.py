import sys
import os

#If user gives a specific set of files from command line
if len(sys.argv)>1:
	testFiles = sys.argv[1:]
else:
	#Get all the files in the tests dir
	testFiles = os.listdir('./tests/')
	testFiles = [ x for x in testFiles if ( x[-3:]=='plt' and ( x[:4] in ['pass','fail'] ))]

nof = len(testFiles)

#passing and failing
passed = []
failed = []
i=0
print 'Starting the tests..'
#Proceed if make succeeds
for file in testFiles:
	#if i%(nof/20) == 0:
	sys.stdout.write('.')
	#For each test file perform the test. And print pass or failure
	runStr = './plt tests/' + file + ' 2> temp.out'
	#print 'Running for file : '+file +'\n'
	os.system(runStr)
	f = open('temp.out')
	s = f.readlines()
	f.close()
	if (len(s)>0 and file[:4]=='pass') or (len(s)==0 and file[:4]=='fail'):
		failed.append('**** FAILED for file '+file+'\n' + ' '.join(s) )
	else:
		passed.append( 'PASSED for --  '+file)
	i+=1
#Printing the results
print '\n---------- PASSED TESTS ------------'
for i in passed:
	print i
print '---------- FAILED TESTS ------------'
for i in failed:
	print i

print '---------- TESTS STATS------------'
print 'Passed : ' + str(len(passed))
print 'Failed : ' + str(len(failed))
