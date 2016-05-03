from subprocess import call
listOfProgram = ["hello.plt", "sum.plt"]
commands  = ["make", "echo \"done\""]
if len(sys.argv) > 1
    call(["make"])

make clean

call(["ls", "-l"])


def runTest(test):
    command = "./plotter < programs/"+test +"> prg_name.cpp"
    call(["make"])
    call(command)
    call("g++ prg_name.cpp")
    call("./a.out")
    arr = test.split(".")
    expectedOUtput = arr[1]+".svg"
    Output="$(diff -B hello.svg test_hello.svg)"
if [ -z $Output ]
then
echo "###### SUCCESS"
else
echo "###### FAILED"
fi
