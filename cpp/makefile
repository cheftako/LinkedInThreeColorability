BASEDIR = .
CC = g++
CPPFLAGS = -ggdb -Wall 
LDFLAGS = -lpthread

all: threecolor

threecolor: threecolor.o node.o threeclique.o

node.o: node.cpp node.h

threeclique.o: threeclique.cpp threeclique.h node.h

threecolor.o: threecolor.cpp node.h

clean:
	rm -rf threecolor threecolor.o node.o threeclique.o *out timer.log
