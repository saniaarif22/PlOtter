import sys
f = open(sys.argv[1])
s = f.readlines()
f.close()

#Get the include list
incLibs = []
codeLines = []
for i in s:
    j=i.strip()
    if len(j.split()) == 2 and j.split()[0]=="include":
        incLibs.append(j.split()[1])
        continue
    codeLines.append(i)

#Get all the library code to be attached
libCode = []
for lib in incLibs:
    f = open("library/"+lib+".plt")
    lc = f.readlines()
    f.close()
    libCode += lc

#Save the original code in .plt_tmp file
fn= open(sys.argv[1] + '_tmp','w')
for item in s:
    fn.write("%s" % item)
fn.close()

#overwrite the new with this code
fullCode = libCode + codeLines
fn= open(sys.argv[1],'w')
for item in fullCode:
    fn.write("%s" % item)
fn.close()
