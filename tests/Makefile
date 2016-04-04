CC  = g++


# Comment out one of the following 2 lines to enable/disable BASIC4TRACE.
INCLUDES = -DBASIC4TRACE
#INCLUDES =

CFLAGS   = -g -Wall $(INCLUDES)
CXXFLAGS = -g -Wall $(INCLUDES)

LDFLAGS = -g
LDLIBS  =

all: 
	g++ helloWorld.cpp -o helloWorld

.PHONY: clean
clean:
	rm -f *~ helloWorld hello.svg
