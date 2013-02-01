CC = gcc -std=c99
CF = -Wall -O2

all : simul

msgconf.o : msgconf.c
	$(CC) $(CF) -c -o msgconf.o msgconf.c

calcs.o : calcs.c
	$(CC) $(CF) -c -o calcs.o calcs.c

inits.o : inits.c
	$(CC) $(CF) -c -o inits.o inits.c

sync.o : sync.c
	$(CC) $(CF) -c -o sync.o sync.c

loyal.o : loyal.c
	$(CC) $(CF) -c -o loyal.o loyal.c

traitor.o : traitor.c
	$(CC) $(CF) -c -o traitor.o traitor.c

ioconf.o : ioconf.c
	$(CC) $(CF) -c -o ioconf.o ioconf.c

simul.o : simul.c
	$(CC) $(CF) -c -o simul.o simul.c

simul : msgconf.o calcs.o inits.o sync.o loyal.o traitor.o ioconf.o simul.o
	$(CC) -o simul msgconf.o calcs.o inits.o sync.o loyal.o traitor.o ioconf.o simul.o

clean :
	rm -f *.o simul
